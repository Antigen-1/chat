#lang scribble/lp2

@title{OpenAI ChatGPT Client}
@author[(author+email "张昊" "zhanghao at antigen dot top")]

@section{Introduction}
[这个应用实际上是针对@hyperlink["https://github.com/sarabander/sicp-pdf/raw/master/sicp.pdf"]{SICP}第三章“从函数式编程的视角看待时间”这个观点的一次实践。本文采用了@hyperlink["https://docs.racket-lang.org/scribble/lp.html"]{文学式编程}的风格，包含了此应用绝大多数源码。还有一小部分源码位于private目录下，是一些stream相关的实用函数，由于未加contract，因此本文档未涉及。]

这个应用实现了基于chatGPT API的简单的文本补全，用户可以优雅的处理输入流、对话上下文和token使用量。

由于难以统计prompt的token数量，这个应用并没有实现流式传输。

@section{上下文和输入流的协议}

需要导入的模块如下。

@CHUNK[<import>
       (require racket/class racket/stream racket/contract json "private/stream.rkt")]

接下来我们要绑定如下这些标识符。

@CHUNK[<export>
       (provide token-logger total-token-logger prompt-token-logger completion-token-logger
                (contract-out (context% (class/c (init-field (model string?)
                                                             (system string?)
                                                             (input (stream/c (or/c 'reset string? (listof string?))))
                                                             (send/recv (-> any/c jsexpr?))
                                                             (prob (-> any/c any)))))))
       ]

导出这些@racket[logger]是为了让用户更方便地获取token的使用量。@racket[context%]是这里的核心，在这里我们用contract的方式规定了各个初始化字段的内容。

@CHUNK[<loggers>
       (code:comment "其他logger的parent logger")
       (define token-logger (make-logger #f (current-logger)))
       (code:comment "token总数")
       (define total-token-logger (make-logger #f token-logger))
       (code:comment "提示的token数量")
       (define prompt-token-logger (make-logger #f token-logger))
       (code:comment "补全的token数量")
       (define completion-token-logger (make-logger #f token-logger))]

@CHUNK[<utilities>
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
       (define (log-tokens t p c #:prefix (prefix ""))
         (define (add-prefix sym) (string->symbol (string-append prefix (symbol->string sym))))

         (log-message total-token-logger 'info (add-prefix 'Tokens) (format "~a" t))
         (log-message prompt-token-logger 'info (add-prefix 'PromptTokens) (format "~a" p))
         (log-message completion-token-logger 'info (add-prefix 'CompletionTokens) (format "~a" c)))]

封装、解析数据包，报告token使用的实用函数。

@CHUNK[<handlers>
       (define (normal history requests)
         (define new-messages (map (lambda (request) (make-message "user" request)) requests))
         (code:comment "Send and receive data")
         (define response
           (send/recv
            (hasheq 'model model
                    'messages (reverse (append new-messages (cdr history))))))
         (code:comment "Inform probes and loggers, and return updated history")
         (let-values (((content) (retrieve-content response))
                      ((total prompt completion) (retrieve-usage response)))
           (prob content)
           (log-tokens total prompt completion)

           (cons (map + (list total prompt completion) (car history))
                 (cons (make-message "assistant" content) (append new-messages (cdr history))))))
       (define (reset history _)
         (code:comment "Conversations are discarded while token usage is preserved")
         (list (car history) (make-message "system" system)))]

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
                                (list (list 0 0 0) (make-message "system" system))
                                (stream-map*
                                 dispatch
                                 history-stream
                                 input))))
           history-stream))]

通过递归定义的@racket[stream]建立反馈回路，整个@racket[stream]就是一个usage和对话的收集器。

以下是@racket[context%]的完整定义。

@CHUNK[<context>
       <import>
       <export>
       <loggers>

       (define context%
         (class object%
           (init-field model system input send/recv prob)

           (super-new)

           <utilities>
           <handlers>
           <dispatcher>
           <accumulator>

           (code:comment "Log the total amount of tokens")
           (keyword-apply log-tokens '(#:prefix) '("All") (car (stream-last (make-history-stream input))))))]

@section{Test}

可以参考下面这个测试用例使用@racket[logger]。

@CHUNK[<test>
       (module* test racket/base
         (code:comment "Any code in this `test` submodule runs when this file is run using DrRacket")
         (code:comment "or with `raco test`. The code here does not run when this file is")
         (code:comment "required by another module.")

         (require rackunit racket/vector racket/class (submod ".."))

         (define log-receiver (make-log-receiver token-logger 'info))

         (void
          (new context%
               (model "gpt-3.5-turbo")
               (system "You are a helpful assistant.")
               (input (in-list (list "Hello." 'reset '("Hello."))))
               (send/recv
                (lambda (js)
                  (hasheq
                   'usage
                   (hasheq 'total_tokens 6
                           'prompt_tokens 6
                           'completion_tokens 0)
                   'choices
                   (list
                    (hasheq
                     'message
                     (hasheq 'content
                             (hash-ref js 'messages)))))))
               (prob (lambda (response)
                       (check-match response
                                    (list (hash-table ('role "system") ('content "You are a helpful assistant."))
                                          (hash-table ('role "user") ('content "Hello."))))))))

         (define (log-message=? v1 v2) (check-equal? (vector-copy v1 0 2) v2))
         (log-message=? (sync log-receiver) (vector 'info "Tokens: 6"))
         (log-message=? (sync log-receiver) (vector 'info "PromptTokens: 6"))
         (log-message=? (sync log-receiver) (vector 'info "CompletionTokens: 0"))
         (log-message=? (sync log-receiver) (vector 'info "Tokens: 6"))
         (log-message=? (sync log-receiver) (vector 'info "PromptTokens: 6"))
         (log-message=? (sync log-receiver) (vector 'info "CompletionTokens: 0"))
         (log-message=? (sync log-receiver) (vector 'info "AllTokens: 12"))
         (log-message=? (sync log-receiver) (vector 'info "AllPromptTokens: 12"))
         (log-message=? (sync log-receiver) (vector 'info "AllCompletionTokens: 0")))]

@section{Commandline}

接下来这个部分是命令行程序，已在info.rkt中注册为@commandline{raco}命令，若要直接运行请使用@commandline{racket -t}。

首先解析命令行参数。这里规定了三种模式。

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{flags} @bold{input}                  @bold{output}                  @bold{interactive?})
               (list "-I"         @racket[(current-input-port)] @racket[(current-output-port)] "N")
               (list "None"       @racket[(current-input-port)] @racket[(current-output-port)] "Y")
               (list "-p <mod>"   "input-stream"                @racket[(current-output-port)] "N"))]

@CHUNK[<commandline>
       (define model (box "gpt-3.5-turbo"))
       (define system (box "You are a helpful assistant."))
       (define interact? (box #t))
       (define token (box #f))
       (define module (box #f))
       (define request-timeout (box 600))
       (define idle-timeout (box 600))
       (define rate-limit (box 6))
       (command-line
        #:program (short-program+command-name)
        #:once-each
        [("-m" "--model") m "Specify the model." (set-box! model m)]
        [("-s" "--system") s "Specify the system prompt." (set-box! system s)]
        [("-I" "--no-interact") "Turn off the interactive mode." (set-box! interact? #f)]
        [("-t" "--token") s "Specify the openai token." (set-box! token s)]
        [("-p" "--module-path") p "Specify the module path to be imported dynamically." (set-box! module (string->path p))]
        [("-r" "--request-timeout") r "Specify how long to wait on a request." (set-box! request-timeout (string->number r))]
        [("-i" "--idle-timeout") i "Specify how long to wait on an idle connection." (set-box! idle-timeout (string->number i))]
        [("-l" "--rate-limit") l "Specify the number of times the client can access the server within a minute." (set-box! rate-limit (string->number l))]
        #:ps
        "The interactive mode is automatically turned off when `-p` or `--module-path` is supplied."
        "The module to be dynamically imported must provide `input-stream` which is a stream of strings, `'reset`s or lists of strings."
        #:args ()
        (code:comment "Checking")
        (cond ((not (unbox token))
               (raise (make-exn:fail:user "You must provide your openai token." (current-continuation-marks)))))
        (for ((timeout (in-list (list request-timeout idle-timeout))))
          (cond ((or (not (unbox timeout)) (not (real? (unbox timeout))) (not (positive? (unbox timeout))))
                 (raise (make-exn:fail:user "Timeouts must be positive numbers." (current-continuation-marks))))))
        (cond ((or (not (unbox rate-limit)) (not (real? (unbox rate-limit))) (not (positive? (unbox rate-limit))))
               (raise (make-exn:fail:user "Rate limits must be positive numbers." (current-continuation-marks))))))]

然后我们使用@hyperlink["https://docs.racket-lang.org/http-easy/index.html"]{http-easy}库绑定与服务器交互的相关函数。

@CHUNK[<communication>
       (code:comment "A procedure used for HTTPS communication")
       (code:comment "Currently HTTP(S) proxies are supported")
       (define send/recv
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
                 (filter-map
                  (lambda (record)
                    (and (cadr record) ((caddr record) (cadr record))))
                  proxy-records))
                (code:comment "Pool and session configuration")
                (pool-config (make-pool-config #:idle-timeout (unbox idle-timeout)))
                (session (make-session #:proxies proxies #:pool-config pool-config))
                (code:comment "Timeout configuration")
                (timeout-config (make-timeout-config #:request (unbox request-timeout)))
                (call/timeout
                 (lambda (proc)
                   (with-handlers ((exn:fail:http-easy:timeout?
                                    (lambda (exn) (raise (make-exn:fail:network
                                                          (format "~a: timed out"
                                                                  (exn:fail:http-easy:timeout-kind exn))
                                                          (current-continuation-marks))))))
                     (proc))))
                (code:comment "The url constant")
                (url (string->url "https://api.openai.com/v1/chat/completions")))
           (plumber-add-flush! (current-plumber) (lambda (_) (session-close! session)))
           (lambda (input)
             (call/timeout
              (lambda ()
                (parameterize ((current-session session))
                  (match
                      (post
                       url
                       #:close? #t
                       #:timeouts timeout-config
                       #:auth (bearer-auth (unbox token))
                       #:data (json-payload input))
                    ((response #:status-code 200
                               #:headers ((content-type (regexp #"application/json")))
                               #:json output)
                     output)
                    ((response #:status-code code #:headers ((content-type type)) #:body body)
                     (raise (make-exn:fail:user (format "code: ~a\ncontent-type: ~a\nbody: ~s" code type body)
                                                (current-continuation-marks)))))))))))]

接下来对输入流作速率限制。这里又使用了@racket[stream]，实际上限制的是每一次输入及其之前输入的平均速率。

@CHUNK[<limit>
       (code:comment "Make input streams with rate limits")
       (define (make-limited-stream input limit)
         (letrec ((delayed-start (delay (current-inexact-milliseconds)))
                  (least-interval (/ 60000.0 limit))
                  (record-stream
                   (stream-cons
                    #:eager (list 0 #f) (code:comment "Adding the interval is unnecessary in the beginning")
                    (stream-map*
                     (lambda (r i)
                       (cond ((or (string? i) (list? i))
                              (sync (handle-evt (alarm-evt (+ (force delayed-start) (* least-interval (car r))))
                                                (lambda (_) (list (add1 (car r)) i)))))
                             (else (list (car r) i))))
                     record-stream
                     input))))
           (stream-filter values (stream-map cadr record-stream))))

       (code:comment "A constructor of context%")
       (define (make-context input)
         (new context%
              (model (unbox model))
              (system (unbox system))
              (input (make-limited-stream input (unbox rate-limit)))
              (send/recv send/recv)
              (prob displayln)))]

最后让我们把main模块组装好吧！

@CHUNK[<main>
       (module* main racket/base
         (code:comment "(Optional) main submodule. Put code here if you need it to be executed when")
         (code:comment "this file is run using DrRacket or the `racket` executable.  The code here")
         (code:comment "does not run when this file is required by another module. Documentation:")
         (code:comment "http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29")

         (require racket/cmdline racket/match racket/list racket/class racket/stream racket/promise
                (submod "..") "private/stream.rkt"
                raco/command-name
                net/http-easy net/url)

         <commandline>
         <communication>
         <limit>

         (code:comment "The main loop")
         (code:comment "The interactive mode works only when `(unbox module)` returns false")
         (void
          (make-context
           (cond ((unbox module) (dynamic-require (unbox module) 'input-stream))
                 (else
                  (cond ((unbox interact?) (displayln (format "I'm ~a. Can I help you?" (unbox model)))))
                  (sequence->stream
                   (in-port (lambda (in)
                              (cond ((unbox interact?) (display "> ")))
                              (read-line in)))))))))]

需要注意的是这个命令行程序有一个设计缺陷，我暂时未找到优雅的解决方法。想要从程序中安全退出只有两种方式。

@itemlist[
          @item{一是输入流结束}
          @item{二是内部发生错误}
          ]

因此你是无法在https连接未断开时安全的中断的（即使使用Ctl-C）。

@section{Outline}

Racket的文学式编程语言要求要有一个提纲把文档所有内容收集起来，从而汇编为可运行的程序。

@CHUNK[
       <*>
       <context>
       <test>
       <main>
       ]
