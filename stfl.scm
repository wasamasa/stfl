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

  (include "stfl-impl.scm"))
