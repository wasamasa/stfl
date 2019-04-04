(import scheme)
(cond-expand
 (chicken-4
  (use extras (prefix stfl stfl:)))
 (chicken-5
  (import (chicken format))
  (import (prefix stfl stfl:))))

(stfl:init!)

(define form (stfl:create "<widgets.stfl>"))

(stfl:set-value! form "value_a" "This is a little")
(stfl:set-value! form "value_b" "test for STFL!")

(let loop ()
  (let ((event (stfl:run! form 0)))
    (cond
     ((equal? event "ESC") #f)
     ((equal? event "^L") (stfl:redraw!) (loop))
     (else (loop)))))

(stfl:reset!)

(display (stfl:get-text form "textedit"))
(printf "A: ~a\n" (stfl:get-value form "value_a"))
(printf "B: ~a\n" (stfl:get-value form "value_b"))
(printf "C: ~a\n" (stfl:get-value form "value_c"))
