(use extras (prefix stfl stfl:))

(define layout #<<EOT
table
  list[list]
    .expand:h
    .border:lrtb
    pos[listpos]:0
    pos_name[listposname]:li0
    listitem[li0] text[li0text]:"ListItem 0"
    listitem[li1] text[li1text]:"ListItem 1"
    listitem[li2] text[li2text]:"ListItem 2"
  tablebr
  label[label]
    .expand:h
    .border:lrtb
    text[labeltext]:
EOT
)

(stfl:init!)

(define form (stfl:create layout))

(let loop ()
  (let* ((event (stfl:run! form 0))
         (pos (stfl:get-value form "listpos"))
         (pos-name (stfl:get-value form "listposname"))
         (text (stfl:get-value form (format "~atext" pos-name)))
         (label-text (format "List is at position ~a, name ~a, text ~a"
                             pos pos-name text)))
    (stfl:set-value! form "labeltext" label-text)
    (cond
     ((equal? event "ESC") #f)
     ((equal? event "^L") (stfl:redraw!) (loop))
     (else (loop)))))
