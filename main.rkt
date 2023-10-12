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

(require racket/class json)

;;Store the context
(define context%
  (class object%
    (init-field model system send recv)

    (super-new)

    (define user-history (box null))
    (define assistant-history (box null))

    ;;Utilities
    (define (insert-history str bx) (set-box! bx (cons str (unbox bx))))
    (define (make-message role cont)
      (hasheq 'role role 'content cont))
    (define (make-conversation user assis)
      (reverse
       (foldl
        (lambda (u a i) (cons (make-message "assistant" a) (cons (make-message "user" u) i)))
        null
        user assis)))
    (define (retrieve-response js)
      (hash-ref
       (hash-ref
        (list-ref
         (hash-ref js 'choices)
         0)
        'message)
       'content))

    (define (step request)
      (define result
        (send
         (jsexpr->bytes
          (hasheq 'model model
                  'messages
                  `(,(make-message "system" system) ;;Set the behaviour of the assistant
                    ,@(make-conversation (unbox user-history) (unbox assistant-history)) ;;Supply previous conversations
                    ,(make-message "user" request) ;;Provide requests or comments for the assistant
                    )))))
      (insert-history request user-history)
      (define response (retrieve-response (bytes->jsexpr (recv result))))
      (insert-history response assistant-history)
      response)
    (public step)))
(define step (generic context% step))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define ctx (new context%
                   (model "gpt-3.5-turbo")
                   (system "You are a helpful assistant.")
                   (send
                    (let-values (((in out) (make-pipe)))
                      (lambda (bstr)
                        (write-json
                         (hasheq
                          'choices
                          (list
                           (hasheq
                            'message
                            (hasheq 'content
                                    (hash-ref (bytes->jsexpr bstr) 'messages)))))
                         out)
                        in)))
                   (recv (lambda (port) (jsexpr->bytes (read-json port))))))
  (define response (send-generic ctx step "Hello."))
  (check-match response
               (list (hash-table ('role "system") ('content "You are a helpful assistant."))
                     (hash-table ('role "user") ('content "Hello.")))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/port racket/stream racket/contract raco/command-name net/url)
  (define model (box "gpt-3.5-turbo"))
  (define system (box "You are a helpful assistant."))
  (define interact? (box #t))
  (define token (box #f))
  (define module (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-m" "--model") m "Specify the model." (set-box! model m)]
    [("-s" "--system") s "Specify the system prompt." (set-box! system s)]
    [("-n" "--no-interact") "Turn off the interactive mode." (set-box! interact? #f)]
    [("-t" "--token") s "Sepcify the openai token." (set-box! token s)]
    [("-p" "--module-path") p "Specify the module path to be imported dynamically." (set-box! module p)]
    #:ps
    "The interactive mode is automatically turned off when `-p` or `--module-path` is supplied."
    "The module to be dynamically imported must provide `input-stream` which is a stream of strings."
    #:args ()
    ;;Check
    (cond ((not (unbox token)) (raise (make-exn:fail:user "You must provide your openai token." (current-continuation-marks)))))
    ;;Cache
    (define url (string->url "https://api.openai.com/v1/chat/completions"))
    (define headers (list "Content-Type: application/json"
                          (format "Authorization: Bearer ~a" (unbox token))))
    ;;Functions
    (define-values (sd rv)
      (values (lambda (bstr)
                (post-impure-port url bstr headers))
              (let ((raise-network (lambda (msg) (raise (make-exn:fail:network msg (current-continuation-marks))))))
                (lambda (port)
                  (let ((header (regexp-match #rx"^HTTP/1\\.[01] ([0-9]+)" (purify-port port))))
                    (cond ((not header) (raise-network "Mis-formatted reply is met."))
                          ((not (string=? (cadr header) "200"))
                           (raise-network (format "HTTP status code: ~a." (cadr header))))
                          (else (port->bytes port))))))))
    ;;History
    (define ctx (new context%
                     (model (unbox model))
                     (system (unbox system))
                     (send sd)
                     (recv rv)))
    ;;REPL
    (with-handlers ((exn:break? void))
      (cond ((unbox module) (define/contract input-stream (stream/c string?) (dynamic-require (unbox module) 'input-stream))
                            (for ((str (in-stream input-stream)))
                              (displayln (send-generic ctx step str))))
            (else
             ;;The interactive mode works only when `(unbox module)` returns false.
             (cond ((unbox interact?) (displayln (format "I'm ~a. Can I help you?" (unbox model)))))
             (for ((line (in-producer (lambda ()
                                        (cond ((unbox interact?) (display "> ")))
                                        (read-line))
                                      eof)))
               (displayln (send-generic ctx step line))))))))
