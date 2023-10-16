#lang racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/class racket/stream racket/contract json "private/stream.rkt")
(provide token-logger prompt-token-logger completion-token-logger
         (contract-out (context% (class/c (init-field (model string?)
                                                      (system string?)
                                                      (input (stream/c (or/c 'reset string? (listof string?))))
                                                      (send/recv (-> any/c jsexpr?))
                                                      (prob (-> any/c any)))))))

;;Loggers
(define token-logger (make-logger #f (current-logger)))
(define prompt-token-logger (make-logger #f token-logger))
(define completion-token-logger (make-logger #f token-logger))

;;Context
(define context%
  (class object%
    (init-field model system input send/recv prob)

    (super-new)

    ;;Utilities
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

      (log-message token-logger 'info (add-prefix 'Tokens) (format "~a" t))
      (log-message prompt-token-logger 'info (add-prefix 'PromptTokens) (format "~a" p))
      (log-message completion-token-logger 'info (add-prefix 'CompletionTokens) (format "~a" c)))

    ;;Handlers
    (define (normal history requests)
      (define new-messages (map (lambda (request) (make-message "user" request)) requests))
      ;;Send and receive data
      (define response
        (send/recv
         (hasheq 'model model
                 'messages (reverse (append new-messages (cdr history))))))
      ;;Inform probes and loggers, and return updated history
      (let-values (((content) (retrieve-content response))
                   ((total prompt completion) (retrieve-usage response)))

        (prob content)
        (log-tokens total prompt completion)

        (cons (map + (list total prompt completion) (car history))
              (cons (make-message "assistant" content) (append new-messages (cdr history))))))
    (define (reset history _)
      ;;Conversations are discarded while token usage is preserved
      (list (car history) (make-message "system" system)))

      (define (make-history-stream input)
        ;;An accumulator represented as a stream
        (letrec ((history-stream
                  (stream-cons #:eager
                               (list (list 0 0 0) (make-message "system" system))
                               (stream-map*
                                (lambda (history request)
                                  ;;Dispatch in terms of the request
                                  (cond ((list? request) (normal history request))
                                        ((string? request) (normal history (list request)))
                                        (else (reset history 'reset))))
                                history-stream
                                input))))
          history-stream))

    ;;Log the total amount of tokens
    (keyword-apply log-tokens '(#:prefix) '("All") (car (stream-last (make-history-stream input))))))

(module* test racket/base
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

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
  (log-message=? (sync log-receiver) (vector 'info "AllCompletionTokens: 0")))

(module* main racket/base
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/match racket/list racket/class racket/stream racket/promise
           (submod "..") "private/stream.rkt"
           raco/command-name
           net/http-easy net/url)
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
    "The module to be dynamically imported must provide `input-stream` which is a stream of strings, `#f`s or lists of strings."
    #:args ()
    ;;Check
    (cond ((not (unbox token))
           (raise (make-exn:fail:user "You must provide your openai token." (current-continuation-marks)))))
    (for ((timeout (in-list (list request-timeout idle-timeout))))
      (cond ((or (not (unbox timeout)) (not (real? (unbox timeout))) (not (positive? (unbox timeout))))
             (raise (make-exn:fail:user "Timeouts must be positive numbers." (current-continuation-marks))))))
    (cond ((or (not (unbox rate-limit)) (not (real? (unbox rate-limit))) (not (positive? (unbox rate-limit))))
           (raise (make-exn:fail:user "Rate limits must be positive numbers." (current-continuation-marks)))))

    ;;A procedure used for HTTPS communication
    ;;Currently HTTP(S) proxies are supported
    (define send/recv
      (let* (;;Proxies
             ;;Records: (listof (list/c <scheme> (or/c #f <server>) <maker>))
             ;;Server: any/c
             ;;Maker: (-> <server> proxy?)
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
             ;;Pool and session configuration
             (pool-config (make-pool-config #:idle-timeout (unbox idle-timeout)))
             (session (make-session #:proxies proxies #:pool-config pool-config))
             ;;Timeout configuration
             (timeout-config (make-timeout-config #:request (unbox request-timeout)))
             (call/timeout
              (lambda (proc)
                (with-handlers ((exn:fail:http-easy:timeout?
                                 (lambda (exn) (raise (make-exn:fail:network
                                                       (format "~a: timed out"
                                                               (exn:fail:http-easy:timeout-kind exn))
                                                       (current-continuation-marks))))))
                  (proc))))
             ;;The url constant
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
                                             (current-continuation-marks)))))))))))

    ;;Make input streams with rate limits
    (define (make-limited-stream input limit)
      (letrec ((delayed-start (delay (current-inexact-milliseconds)))
               (least-interval (/ 60000.0 limit))
               (record-stream
                (stream-cons
                 #:eager (list 0 #f) ;;Adding the interval is unnecessary in the beginning
                 (stream-map*
                  (lambda (r i)
                    (cond ((or (string? i) (list? i))
                           (sync (handle-evt (alarm-evt (+ (force delayed-start) (* least-interval (car r))))
                                             (lambda (_) (list (add1 (car r)) i)))))
                          (else (list (car r) i))))
                  record-stream
                  input))))
        (stream-filter values (stream-map cadr record-stream))))

    ;;A constructor of context%
    (define (make-context input)
      (new context%
           (model (unbox model))
           (system (unbox system))
           (input (make-limited-stream input (unbox rate-limit)))
           (send/recv send/recv)
           (prob displayln)))

    ;;The main loop
    ;;The interactive mode works only when `(unbox module)` returns false
    (void
     (make-context
      (cond ((unbox module) (dynamic-require (unbox module) 'input-stream))
            (else
             (cond ((unbox interact?) (displayln (format "I'm ~a. Can I help you?" (unbox model)))))
             (sequence->stream
              (in-port (lambda (in)
                         (cond ((unbox interact?) (display "> ")))
                         (read-line in))))))))))
