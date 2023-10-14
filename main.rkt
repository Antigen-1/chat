#lang racket/base

(module+ test
  (require rackunit))

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

(require racket/class racket/stream)
(provide token-logger total-token-logger prompt-token-logger completion-token-logger)

;;Loggers
(define token-logger (make-logger #f (current-logger)))
(define total-token-logger (make-logger #f token-logger))
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
    (define (stream-map* proc . ss)
      (if (ormap stream-empty? ss)
          empty-stream
          (stream-cons #:eager (apply proc (map stream-first ss))
                       (apply stream-map* proc (map stream-rest ss)))))

    ;;A signal-processing system represented as an infinite stream
    (define (make-history-stream input)
      (letrec ((history-stream
                (stream-cons #:eager
                             (list 0 (make-message "system" system))
                             (stream-map*
                              (lambda (history request)
                                (define new-message (make-message "user" request))
                                ;;Send and receive
                                (define response
                                  (send/recv
                                   (hasheq 'model model
                                           'messages (reverse (cons new-message (cdr history))))))
                                ;;Inform probes and loggers, and return updated history
                                (let-values (((content) (retrieve-content response))
                                             ((total prompt completion) (retrieve-usage response)))

                                  (prob content)
                                  (log-message total-token-logger 'info 'TotalTokens (format "~a" total))
                                  (log-message prompt-token-logger 'info 'PromptTokens (format "~a" prompt))
                                  (log-message completion-token-logger 'info 'CompletionTokens (format "~a" completion))

                                  (cons (+ total (car history)) (cons (make-message "assistant" content) (cons new-message (cdr history))))))
                              history-stream
                              input))))
        history-stream))

    ;;Log the total amount of all tokens
    (log-message token-logger 'info 'AllTokens (format "~a" (caar (reverse (stream->list (make-history-stream input))))))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require racket/vector)

  (define log-receiver (make-log-receiver token-logger 'info))

  (void
   (new context%
        (model "gpt-3.5-turbo")
        (system "You are a helpful assistant.")
        (input (in-list (list "Hello.")))
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
  (log-message=? (sync log-receiver) (vector 'info "TotalTokens: 6"))
  (log-message=? (sync log-receiver) (vector 'info "PromptTokens: 6"))
  (log-message=? (sync log-receiver) (vector 'info "CompletionTokens: 0"))
  (log-message=? (sync log-receiver) (vector 'info "AllTokens: 6")))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract racket/match
           raco/command-name
           net/http-easy net/url net/cookies)
  (define model (box "gpt-3.5-turbo"))
  (define system (box "You are a helpful assistant."))
  (define interact? (box #t))
  (define token (box #f))
  (define module (box #f))
  (define timeouts (box 60))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-m" "--model") m "Specify the model." (set-box! model m)]
    [("-s" "--system") s "Specify the system prompt." (set-box! system s)]
    [("-n" "--no-interact") "Turn off the interactive mode." (set-box! interact? #f)]
    [("-t" "--token") s "Specify the openai token." (set-box! token s)]
    [("-p" "--module-path") p "Specify the module path to be imported dynamically." (set-box! module (string->path p))]
    [("-T" "--timeouts") t "Configure the timeouts." (set-box! timeouts (string->number t))]
    #:ps
    "The interactive mode is automatically turned off when `-p` or `--module-path` is supplied."
    "The module to be dynamically imported must provide `input-stream` which is a stream of strings."
    #:args ()
    ;;Check
    (cond ((not (unbox token)) (raise (make-exn:fail:user "You must provide your openai token." (current-continuation-marks)))))
    (cond ((or (not (unbox timeouts)) (not (real? (unbox timeouts))) (not (positive? (unbox timeouts)))) (raise (make-exn:fail:user "Timeouts must be positive numbers." (current-continuation-marks)))))

    ;;A procedure used for HTTPS communication
    ;;Support proxies and cookie storage
    (define-values (session send/recv)
      (let* ((proxy-server (proxy-server-for "https"))
             (proxy (if proxy-server
                        (match proxy-server
                          ((list scheme host port)
                           (make-https-proxy
                            (url->string
                             (make-url scheme
                                       #f
                                       host
                                       port
                                       #f
                                       null
                                       null
                                       #f)))))
                        null))
             (jar (new list-cookie-jar%))
             (timeout-config (make-timeout-config #:lease (unbox timeouts) #:connect (unbox timeouts) #:request (unbox timeouts)))
             (session (make-session #:proxies (list proxy) #:cookie-jar jar))
             (url "https://api.openai.com/v1/chat/completions"))
        (values
         session
         (lambda (input)
           (match
               (session-request
                session url
                #:timeouts timeout-config
                #:auth (bearer-auth (unbox token))
                #:method 'post
                #:data (json-payload input))
             ((response #:status-code 200
                        #:headers ((content-type (regexp #"application/json")))
                        #:json output)
              output))))))

    ;;A constructor of context%
    (define (make-context input)
      (new context%
           (model (unbox model))
           (system (unbox system))
           (input input)
           (send/recv send/recv)
           (prob displayln)))

    ;;The main loop
    ;;The interactive mode works only when `(unbox module)` returns false
    (with-handlers ((exn:break? void))
      (void
       (make-context
        (cond ((unbox module)
               (define/contract input-stream (stream/c string?) (dynamic-require (unbox module) 'input-stream))
               input-stream)
              (else
               (cond ((unbox interact?) (displayln (format "I'm ~a. Can I help you?" (unbox model)))))
               (sequence->stream
                (in-port (lambda (in)
                           (cond ((unbox interact?) (display "> ")))
                           (read-line in)))))))))

    ;;Finalization
    (session-close! session)))
