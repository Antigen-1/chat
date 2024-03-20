#lang scribble/lp2

@title{OpenAI ChatGPT Client}
@author[(author+email "张昊" "zhanghao at antigen dot top")]

@require[(for-label racket net/http-easy net/url json)]

@section{Introduction}
[这个应用实际上是针对@hyperlink["https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html"]{SICP}
第三章“从函数式编程的视角看待时间”这个观点的一次实践。本文采用了@hyperlink["https://docs.racket-lang.org/scribble/lp.html"]{文学式编程}的风格，
包含了此应用绝大多数源码。还有一小部分源码位于private目录下，由于未加contract，因此本文档未涉及。]

这个应用实现了基于chatGPT API的简单的文本补全，用户可以优雅的处理输入流、对话上下文和token使用量。

由于难以统计prompt的token数量，这个应用并没有实现流式传输。

@section{Plugin System}

@CHUNK[<core-plugin-system>
       (module core-pkg racket/base
         (require racket/contract)
         (provide (contract-out
                   #:forall (input-item? request? response? output-item?)
                   (struct core
                     ((system-prompt (-> input-item?))
                      (make-request-json (-> string? (listof input-item?) request?))
                      (retrieve-content-from-json (-> response? output-item?))
                      (retrieve-usage-from-json (-> response? (values exact-nonnegative-integer?
                                                                      exact-nonnegative-integer?
                                                                      exact-nonnegative-integer?)))
                      (merge-new-content-to-history (-> (or/c 'requests 'response)
                                                        (listof output-item?)
                                                        (listof input-item?)
                                                        (listof input-item?)))
                      (send/recv (-> (-> (or/c #f string?) any) request? response?))))
                   (put (-> (and/c string? (lambda (nm) (not (hash-has-key? pkgs nm))))
                            core?
                            any))
                   (get (-> string? core?))))
         (struct core (system-prompt
                       make-request-json
                       retrieve-content-from-json
                       retrieve-usage-from-json
                       merge-new-content-to-history
                       send/recv))
         (define pkgs (make-hash))
         (define (put name core) (hash-set! pkgs name core))
         (define (get name) (hash-ref pkgs name)))]

所有核心组件都通过这个包管理系统来注册和使用。在这里通过@racket[contract]定义了@deftech{core}。

@section{Context and Protocol}

接下来我们要绑定如下这些标识符。

@CHUNK[<export>
       (provide token-logger total-token-logger prompt-token-logger completion-token-logger retry-logger
                (contract-out (context% (class/c (init-field (model string?)
                                                             (input (stream/c (or/c 'reset string? (listof string?))))
                                                             (probe (-> any/c any))
                                                             (retry-limit exact-nonnegative-integer?)
                                                             (core-structure core?)
                                                             ))))
                (struct-out exn:fail:chat)
                (struct-out exn:fail:chat:retry-limit))
       ]

导出这些@racket[logger]是为了让用户更方便地获取token的使用量。
@racket[context%]是这里的核心，在这里我们用contract的方式规定了各个初始化字段的内容。
此外还导出了一些新的异常以便用户作异常处理。

@CHUNK[<loggers>
       (code:comment "其他logger的parent logger")
       (define token-logger (make-logger #f (current-logger)))
       (code:comment "token总数")
       (define total-token-logger (make-logger #f token-logger))
       (code:comment "提示的token数量")
       (define prompt-token-logger (make-logger #f token-logger))
       (code:comment "补全的token数量")
       (define completion-token-logger (make-logger #f token-logger))
       (code:comment "重试原因")
       (define retry-logger (make-logger #f (current-logger)))]

@CHUNK[<utilities>
       (define (return-fail msg)
         (define (report str) (log-message retry-logger 'info 'Retry str))
         (define (message->string msg) (if msg msg "unknown"))
         (define str (message->string msg))
         (report str)
         (code:comment "The value field is always a string")
         (Left str))
       (define (log-tokens t p c #:prefix (prefix ""))
         (define (add-prefix sym) (string->symbol (string-append prefix (symbol->string sym))))

         (log-message total-token-logger 'info (add-prefix 'Tokens) (format "~a" t))
         (log-message prompt-token-logger 'info (add-prefix 'PromptTokens) (format "~a" p))
         (log-message completion-token-logger 'info (add-prefix 'CompletionTokens) (format "~a" c)))]

封装、解析数据包，报告token使用和异常的实用函数。

@CHUNK[<handlers>
       (define (normal history requests)
         (let/cc cc
           (code:comment "Send and receive data")
           (define new-history (merge-new-content-to-history 'requests requests (cdr history)))
           (define response
             (send/recv
              (code:comment "Retry occurs only when send/recv wants to raise an exception")
              (lambda ((msg #f)) (cc (return-fail msg)))
              (make-request-json model new-history)))
           (code:comment "Inform probes and loggers, and return updated history")
           (let-values (((content) (retrieve-content-from-json response))
                        ((total prompt completion) (retrieve-usage-from-json response)))
             (probe content)
             (log-tokens total prompt completion)

             (Right (cons (map + (list total prompt completion) (car history))
                          (merge-new-content-to-history 'response (list content) new-history))))))
       (define (reset history _)
         (code:comment "Conversations are discarded while token usage is preserved")
         (Right (list (car history) (system-prompt))))]

在这里定义两种事件。

@itemlist[
          @item{@racket[normal]:与服务器交互}
          @item{@racket[reset]:重置上下文}
          ]

@CHUNK[<dispatcher>
       (define (dispatch history request)
         (code:comment "Dispatch in terms of the request")
         (cond ((list? request) (normal history request))
               ((string? request) (normal history (list request)))
               (else (reset history 'reset))))]

调度两种事件的处理。具体协议见前文contract。

@CHUNK[<accumulator>
       (define (make-history-stream input)
         (code:comment "An accumulator represented as a stream")
         (letrec ((history-stream
                   (stream-cons #:eager
                                (list (list 0 0 0) (system-prompt))
                                (stream-map*
                                 (lambda (hs rq) (retry retry-limit (lambda () (dispatch hs rq))))
                                 history-stream
                                 input))))
           history-stream))]

通过递归定义的@racket[stream]建立反馈回路，整个@racket[stream]就是一个usage和对话的收集器。

以下是@racket[context%]的完整定义。

@CHUNK[<context>
       (module* context racket/base
         (require racket/base racket/class racket/stream racket/contract racket/match
                  (submod ".." core-pkg)
                  "private/stream.rkt" "private/error.rkt" "private/retry.rkt")
         <export>
         <loggers>

         (define context%
           (class object%
             (init-field model input probe retry-limit
                         core-structure)

             (super-new)

             (match-define (core system-prompt
                                 make-request-json
                                 retrieve-content-from-json
                                 retrieve-usage-from-json
                                 merge-new-content-to-history
                                 send/recv)
               core-structure)

             <utilities>
             <handlers>
             <dispatcher>
             <accumulator>

             (code:comment "Log the total amount of tokens")
             (keyword-apply log-tokens '(#:prefix) '("All") (car (stream-last (make-history-stream input)))))))]

@section{Test}

可以参考下面这个测试用例使用@racket[logger]和设计新的@tech{core}。你也可以根据它理解整个程序的工作流程。

@CHUNK[<test>
       (module* test racket/base
         (code:comment "Any code in this `test` submodule runs when this file is run using DrRacket")
         (code:comment "or with `raco test`. The code here does not run when this file is")
         (code:comment "required by another module.")

         (require rackunit
                  racket/generator racket/vector racket/class
                  (submod ".." context) (submod ".." core-pkg))

         (define retry-log-receiver (make-log-receiver retry-logger 'info))
         (define token-log-receiver (make-log-receiver token-logger 'info))

         (define int-generator (generator () (let loop ((i 0)) (yield i) (loop (add1 i)))))
         (define (send/recv left js)
           (define int (int-generator))
           (if (and (>= int 3) (<= int 4))
               js
               (left "a")))
         (define (probe v) (void))
         (define model "gpt-3.5-turbo")
         (define input (in-list (list "1" (list "2" "3") 'reset)))

         (define tt 12)
         (define pt 6)
         (define ct 6)

         (define core-structure
           (core
            (lambda () "system prompt")
            (lambda (_ history) history)
            (lambda (_) "Hello!")
            (lambda (_) (values tt pt ct))
            (lambda (_ requests history)
              (append history requests))
            send/recv))
         (define name (symbol->string (gensym 'core)))
         (put name core-structure)

         (define (make-context limit)
           (new context%
                (model model)
                (input input)
                (probe probe)
                (retry-limit limit)
                (core-structure (get name))))

         (void (make-context 3))

         (define (log-message=? v1 v2) (check-equal? (vector-copy v1 0 2) v2))
         (log-message=? (sync retry-log-receiver) (vector 'info "Retry: a"))
         (log-message=? (sync retry-log-receiver) (vector 'info "Retry: a"))
         (log-message=? (sync retry-log-receiver) (vector 'info "Retry: a"))
         (log-message=? (sync token-log-receiver) (vector 'info (format "Tokens: ~a" tt)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "PromptTokens: ~a" pt)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "CompletionTokens: ~a" ct)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "Tokens: ~a" tt)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "PromptTokens: ~a" pt)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "CompletionTokens: ~a" ct)))
         (log-message=? (sync token-log-receiver) (vector 'info (format "AllTokens: ~a" (* 2 tt))))
         (log-message=? (sync token-log-receiver) (vector 'info (format "AllPromptTokens: ~a" (* 2 pt))))
         (log-message=? (sync token-log-receiver) (vector 'info (format "AllCompletionTokens: ~a" (* 2 ct))))

         (check-exn
          (lambda (e)
            (and (exn:fail:chat:retry-limit? e)
                 (string=? (exn-message e)
                           "make-retry: hit the limit\nDepth: 7\nIts last attempt fails due to:\n\ta")))
          (lambda () (make-context 6))))]

@section{Configuration}

这个部分主要是配置程序运行的环境，包括很多参数，可以通过这些源代码了解哪些参数必须提供、哪些参数有默认值以及各个参数应该设置为什么值。

之所以单独设置一个模块，一方面是为了提供除命令行参数以外另一种配置方式，使配置更灵活（例如@racket[extra-proxies]、@racket[module]和@racket[probe]）；另一方面则是为了便于检查。

@CHUNK[<configuration>
       (module config racket/base
         (require racket/contract net/http-easy)
         (provide (contract-out
                   (url-prefix (box/c string?))
                   (core-name (box/c string?))
                   (extra-proxies (box/c (listof proxy?)))
                   (model (box/c string?))
                   (system (box/c string?))
                   (interact? (box/c boolean?))
                   (token (box/c (or/c #f string?)))
                   (module (box/c (or/c #f module-path?)))
                   (probe (box/c (-> any/c any)))
                   (request-timeout (box/c (and/c real? positive?)))
                   (idle-timeout (box/c (and/c real? positive?)))
                   (rate-limit (box/c (and/c real? positive?)))
                   (retry-limit (box/c exact-nonnegative-integer?))))
         (define url-prefix (box "https://api.openai.com"))
         (define core-name (box "default"))
         (define extra-proxies (box null))
         (define model (box "gpt-3.5-turbo"))
         (define system (box "You are a helpful assistant."))
         (define interact? (box #t))
         (define token (box #f))
         (define module (box #f))
         (define probe (box displayln))
         (define request-timeout (box 600))
         (define idle-timeout (box 600))
         (define rate-limit (box 2))
         (define retry-limit (box 2)))]

@section{Communication}

我们使用@hyperlink["https://docs.racket-lang.org/http-easy/index.html"]{http-easy}库绑定与服务器交互的相关函数。

@racket[make-send/recv]的例子见@italic{Default Core}一节。

@CHUNK[<communication>
       (module* communication racket/base
         (require racket/match racket/list
                  net/http-easy net/url
                  (submod ".." config))
         (provide send/recv-wrapper)

         (code:comment "A procedure used to wrap make-send/recv")
         (code:comment "Currently HTTP(S) proxies are supported")
         (define (send/recv-wrapper make-send/recv)
             (let* ((code:comment "Proxies")
                    (code:comment "Records: (listof (list/c <scheme> (or/c #f <server>) <maker>))")
                    (code:comment "Server: any/c")
                    (code:comment "Maker: (-> <server> proxy?)")
                    (format-http*-proxy-server
                     (lambda (server)
                       (match server
                         ((list scheme host port)
                          (url->string
                           (make-url scheme
                                     #f
                                     host
                                     port
                                     #f
                                     null
                                     null
                                     #f))))))
                    (proxy-records (list (list "http" (proxy-server-for "http") (compose1 make-http-proxy format-http*-proxy-server))
                                         (list "https" (proxy-server-for "https") (compose1 make-https-proxy format-http*-proxy-server))))
                    (proxies
                     (append (unbox extra-proxies)
                             (filter-map
                              (lambda (record)
                                (and (cadr record) ((caddr record) (cadr record))))
                              proxy-records)))
                    (code:comment "Pool and session configuration")
                    (pool-config (make-pool-config #:idle-timeout (unbox idle-timeout)))
                    (session (make-session #:proxies proxies #:pool-config pool-config))
                    (code:comment "Timeout configuration")
                    (timeout-config (make-timeout-config #:request (unbox request-timeout)))
                    (call/handler
                     (lambda (fail proc)
                       (with-handlers ((exn:fail:http-easy:timeout?
                                        (lambda (exn) (fail (format "~a: timed out" (exn:fail:http-easy:timeout-kind exn)))))
                                       (exn:fail:http-easy? (lambda (exn) (fail (exn-message exn)))))
                         (proc))))
                    (code:comment "The url constant")
                    (url (string->url (string-append (unbox url-prefix) "/v1/chat/completions")))
                    (code:comment "A normal send/recv function")
                    (send/recv (make-send/recv timeout-config (unbox token) url)))
               (plumber-add-flush! (current-plumber) (lambda (_) (session-close! session)))
               (code:comment "A send/recv function running in a proper environment")
               (lambda (fail input)
                 (call/handler
                  fail
                  (lambda ()
                    (parameterize ((current-session session))
                      (send/recv fail input))))))))]

@section{Default Core}

默认的@tech{core}实现如下。这个@tech{core}实现了与@italic{openai api}一般的交互。
之所以说是一般，是因为没有实现@italic{streaming}。
原因有很多，最重要的一点是流式传输无法使用racket实现完整的token统计。

如果用户需要自定义一套实用的@tech{core}，可以参考这个模块。

@CHUNK[<default>
       (module* default-core-pkg racket/base
         (require
          racket/match
          net/http-easy
          (submod ".." communication)
          (rename-in (only-in (submod ".." core-pkg) core put) (put pkg-put))
          (submod ".." config))
         (provide install-default)

         (code:comment "Utilities")
         (define (make-message role cont)
           (hasheq 'role role 'content cont))
         (define (retrieve-content js)
           (hash-ref
            (hash-ref
             (list-ref
              (hash-ref js 'choices)
              0)
             'message)
            'content))
         (define (retrieve-usage js)
           (let ((table (hash-ref js 'usage)))
             (values (hash-ref table 'total_tokens)
                     (hash-ref table 'prompt_tokens)
                     (hash-ref table 'completion_tokens))))
         (define (make-request model history)
           (hasheq 'model model 'messages history))
         (define (merge mode requests history)
           (append history
                   (map
                    (lambda (request) (make-message (if (eq? mode 'requests) "user" "assistant") request))
                    requests)))
         (define ((make-send/recv timeout-config token url) fail input)
           (match
               (post
                url
                #:close? #t
                #:timeouts timeout-config
                #:auth (bearer-auth token)
                #:data (json-payload input))
             ((response #:status-code 200
                        #:headers ((content-type (regexp #"application/json")))
                        #:json output)
              output)
             ((response #:status-code code #:headers ((content-type type)) #:body body)
              (fail (format "code: ~a\ncontent-type: ~a\nbody: ~s" code type body)))))

         (define (install-default)
           (define core-structure (core (lambda () (make-message "system" (unbox system)))
                                        make-request
                                        retrieve-content
                                        retrieve-usage
                                        merge
                                        (send/recv-wrapper make-send/recv)))
           (pkg-put "default" core-structure)))]

@section{Commandline}

接下来这个部分是命令行程序，已在info.rkt中注册为@commandline{raco}命令，若要直接运行请使用@commandline{racket -t}。

首先解析命令行参数。这里规定了三种模式。

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{flags} @bold{input}                  @bold{interactive?})
               (list "-I"         @racket[(current-input-port)] "N")
               (list "None"       @racket[(current-input-port)] "Y")
               (list "-p <mod>"   "input-stream"                "N"))]

在这里对比一下@racket[module]和@racket[patch]：

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{类型} @bold{作用} @bold{要求}                          @bold{加载方式})
               (list "module"   "提供输入流" "必须provide一个input-stream作为输入流" "命令行设置或直接设置参数")
               (list "patch"    "任意用途"   "无要求，只要是个racket模块即可"         "只能通过命令行设置"))]

关于加载的过程，见@racket[dynamic-require]。

必需的参数这里也进行了检查。
@racket[patch]在检查之前执行，因此无法脱离检查。
而@racket[module]在检查后导入，因此在下面构建输入流时，需使用@racket[stream-lazy]，需要使用输入流时再导入模块，这样@racket[module]当中设置的参数就无效了。

@CHUNK[<commandline>
       (command-line
        #:program (short-program+command-name)
        #:once-each
        [("-u" "--url") u "Specify the URL prefix." (set-box! url-prefix u)]
        [("-C" "--core") c "Specify the core." (set-box! core-name c)]
        [("-m" "--model") m "Specify the model." (set-box! model m)]
        [("-s" "--system") s "Specify the system prompt." (set-box! system s)]
        [("-I" "--no-interact") "Turn off the interactive mode." (set-box! interact? #f)]
        [("-t" "--token") s "Specify the openai token." (set-box! token s)]
        [("-p" "--module") p "Specify the module to be imported dynamically." (set-box! module (string->path p))]
        [("-c" "--patch") p "Specify the patch to be executed dynamically." (dynamic-require p #f)]
        [("-r" "--request-timeout") r "Specify how long to wait on a request." (set-box! request-timeout (string->number r))]
        [("-i" "--idle-timeout") i "Specify how long to wait on an idle connection." (set-box! idle-timeout (string->number i))]
        [("-l" "--rate-limit") l "Specify the number of times the client can access the server within a minute." (set-box! rate-limit (string->number l))]
        [("-n" "--retry-limit") l "Specify the number of times the client can re-send a request." (set-box! retry-limit (string->number l))]
        #:ps
        "1. The interactive mode is automatically turned off when `-p` or `--module-path` is supplied."
        "2. The module to be dynamically imported must provide `input-stream` which is a stream of strings, `'reset`s or lists of strings."
        "3. The patch to be dynamically executed is an arbitrary racket module and you can configure the program in this file."
        "4. You can use commas to input commands and blocks when running the driver loop in the interactive mode."
        #:args ()
        (code:comment "Additional checks")
        (cond ((not (unbox token)) (raise (make-exn:fail:user "You must provide your openai token." (current-continuation-marks)))))
        )
       ]

接下来对输入流作速率限制。这里又使用了@racket[stream]，实际上限制的是每一次输入及其之前输入的平均速率。

@CHUNK[<limit>
       (code:comment "Make input streams with rate limits")
       (define (make-limited-stream input limit)
         (letrec ((delayed-start (delay (current-inexact-milliseconds)))
                  (least-interval (/ 60000.0 limit))
                  (record-stream
                   (stream-cons
                    (code:comment "Adding the interval is unnecessary in the beginning")
                    #:eager (list 0 'reset)
                    (stream-map*
                     (lambda (r i)
                       (cond ((or (string? i) (list? i))
                              (sync (handle-evt (alarm-evt (+ (force delayed-start) (* least-interval (car r))))
                                                (lambda (_) (list (add1 (car r)) i)))))
                             (else (list (car r) i))))
                     record-stream
                     input))))
           (stream-map cadr record-stream)))]

driver loop在这里直接用输入流表示，如前所述，一种是通过模块导入，一种是从标准输入读取。

@CHUNK[<input>
       (define (handle-line line)
         (match line
           ((regexp #rx"^,(.*)$" (list _ datum-string)) (read (open-input-string datum-string)))
           (_ line)))
       (define input-stream
         (cond ((unbox module)
                (code:comment "The module is loaded when the stream is needed.")
                (stream-lazy (dynamic-require (unbox module) 'input-stream)))
               (else
                (code:comment "The interactive mode works only when `(unbox module)` returns false")
                (cond ((unbox interact?) (displayln (format "I'm ~a. Can I help you?" (unbox model)))))
                (letrec ((read-requests (lambda (in)
                                          (cond ((unbox interact?) (display "> ")))
                                          (define line (handle-line (read-line in)))
                                          (if (eof-object? line) empty-stream (stream-cons #:eager line (read-requests in))))))
                  (read-requests (current-input-port))))))]

最后让我们把main模块组装好吧！

@CHUNK[<main>
       (module* main racket/base
         (code:comment "(Optional) main submodule. Put code here if you need it to be executed when")
         (code:comment "this file is run using DrRacket or the `racket` executable.  The code here")
         (code:comment "does not run when this file is required by another module. Documentation:")
         (code:comment "http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29")

         (require racket/cmdline racket/match racket/class racket/stream racket/promise
                  (submod ".." context) (submod ".." config) (submod ".." default-core-pkg)
                  (rename-in (only-in (submod ".." core-pkg) get) (get pkg-get))
                  "private/stream.rkt"
                  raco/command-name)

         <commandline>
         <limit>
         <input>

         (code:comment "Install the default package")
         (install-default)

         (void
          (new context%
               (model (unbox model))
               (retry-limit (unbox retry-limit))
               (input (make-limited-stream input-stream (unbox rate-limit)))
               (probe (unbox probe))
               (core-structure (pkg-get (unbox core-name))))))]

从这里可以发现，想要从程序中安全退出有且只有一种方式，即终止输入流。

@section{Outline}

Racket的文学式编程语言要求要有一个提纲把文档所有内容收集起来，从而汇编为可运行的程序。

@CHUNK[
       <*>
       <core-plugin-system>
       <context>
       <test>
       <configuration>
       <communication>
       <default>
       <main>
       ]

@section{Release Notes}

@itemlist[
          @item{2023.12 添加了重试的功能，改进了配置、异常处理和程序退出。}
          @item{2024.01 改良了交互模式。}
          @item{2024.03 实现了插件系统。}
          ]
