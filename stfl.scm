(module stfl
  (init! create clean-up!
   run! reset! redraw!
   get-value set-value!
   get-focus set-focus!
   quote-text get-text
   dump modify!
   get-error set-error-action!)

  (import scheme)
  (cond-expand
   (chicken-4
    (import chicken foreign)
    (use lolevel))
   (chicken-5
    (import (chicken base))
    (import (chicken condition))
    (import (chicken foreign))
    (import (chicken format))
    (import (chicken gc))
    (import (chicken memory))))

  (include "stfl-impl.scm"))
