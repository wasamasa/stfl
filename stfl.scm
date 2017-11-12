(module stfl
  (init! create clean-up!
   run! reset! redraw!
   get-value set-value!
   get-focus set-focus!
   quote-text get-text
   dump modify!
   get-error set-error-action!)

(import chicken scheme foreign)
(use lolevel)

#> #include "stfl.h" <#
;; adapted from swig/basedecls.i
#> #include "stfl_wrap.c" <#

;; typedefs

(define-foreign-type string nonnull-c-string)
(define-foreign-type nullable-string c-string)
(define-foreign-type stfl_form* (nonnull-c-pointer (struct "stfl_form")))

;; foreign functions

(define ipool_destroy (foreign-lambda void "ipool_destroy"))
(define stfl_init (foreign-lambda void "stfl_init"))
(define stfl_create (foreign-lambda stfl_form* "stfl_create_wrapper" string))
(define stfl_free (foreign-lambda void "stfl_free_wrapper" stfl_form*))
(define stfl_run (foreign-lambda nullable-string "stfl_run_wrapper" stfl_form* int))
(define stfl_get (foreign-lambda nullable-string "stfl_get_wrapper" stfl_form* string))
(define stfl_set (foreign-lambda void "stfl_set_wrapper" stfl_form* string string))
(define stfl_get_focus (foreign-lambda nullable-string "stfl_get_focus_wrapper" stfl_form*))
(define stfl_set_focus (foreign-lambda void "stfl_set_focus_wrapper" stfl_form* string))
(define stfl_quote (foreign-lambda string "stfl_quote_wrapper" string))
(define stfl_dump (foreign-lambda nullable-string "stfl_dump_wrapper" stfl_form* nullable-string nullable-string bool))
(define stfl_text (foreign-lambda nullable-string "stfl_text_wrapper" stfl_form* string))
(define stfl_modify (foreign-lambda void "stfl_modify_wrapper" stfl_form* string string nullable-string))
(define stfl_error (foreign-lambda nullable-string "stfl_error_wrapper"))
(define stfl_error_action (foreign-lambda void "stfl_error_action_wrapper" string))
(define stfl_reset (foreign-lambda void "stfl_reset"))
(define stfl_redraw (foreign-lambda void "stfl_redraw"))

;; form record

(define (format-pointer pointer)
  (if pointer
      (sprintf "0x~x" (pointer->address pointer))
      "NULL"))

(define-record form pointer)
(define-record-printer (form f out)
  (fprintf out "#<form ~a>"
           (format-pointer (form-pointer f))))

;; API

(define (init!)
  (stfl_init)

  (let ((old (current-exception-handler)))
    (current-exception-handler
     (lambda (ex)
       (clean-up!)
       (old ex))))

  (on-exit clean-up!))

(define (create text)
  (set-finalizer! (make-form (stfl_create text)) release!))

(define (clean-up!)
  (stfl_reset)
  (ipool_destroy))

(define (release! form)
  (and-let* ((form* (form-pointer form)))
    (stfl_free form*)
    (form-pointer-set! form #f)))

(define (run! form timeout)
  (and-let* ((form* (form-pointer form)))
    (stfl_run form* timeout)))

(define (redraw!)
  (stfl_redraw))

(define (reset!)
  (stfl_reset))

(define (get-value form name)
  (and-let* ((form* (form-pointer form)))
    (stfl_get form* name)))

(define (set-value! form name value)
  (and-let* ((form* (form-pointer form)))
    (stfl_set form* name value)))

(define (get-focus form)
  (and-let* ((form* (form-pointer form)))
    (stfl_get_focus form*)))

(define (set-focus! form name)
  (and-let* ((form* (form-pointer form)))
    (stfl_set_focus form* name)))

(define (quote-text text)
  (stfl_quote text))

(define (get-text form name)
  (and-let* ((form* (form-pointer form)))
    (stfl_text form* name)))

(define (dump form #!key name prefix focus)
  (and-let* ((form* (form-pointer form)))
    (stfl_dump form* name prefix focus)))

(define (modify! form name mode #!optional text)
  (and-let* ((form* (form-pointer form)))
    (stfl_modify form* name mode text)))

(define (get-error)
  (stfl_error))

(define (set-error-action! mode)
  (stfl_error_action mode))

)
