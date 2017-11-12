(use extras (prefix stfl stfl:))

(define layout #<<EOT
vbox
  @style_normal:bg=blue
  table
    label
      .colspan:2 .expand:0 .tie:c
      text:"vCard Creator"
    tablebr
    label
      .border:ltb .expand:0
      text:"First Name: "
    input[firstname]
      .border:rtb .expand:h
      style_normal:attr=underline
      style_focus:fg=red,attr=underline
      text[firstnametext]:
    tablebr
    label
      .border:ltb .expand:0
      text:"Last Name: "
    input[lastname]
      .border:rtb .expand:h
      style_normal:attr=underline
      style_focus:fg=red,attr=underline
      text[lastnametext]:
    tablebr
    label
      .border:ltb .expand:0
      text:"Email Address: "
    input[email]
      .border:rtb .expand:h
      style_normal:attr=underline
      style_focus:fg=red,attr=underline
      text[emailtext]:
    tablebr
    label
      .border:ltb .expand:0
      text:"Save to File: "
    input[file]
      .border:rtb .expand:h
      style_normal:attr=underline
      style_focus:fg=red,attr=underline
      text[filetext]:
    tablebr
    label
      .colspan:2 .expand:0 .tie:r
      text[msg]:
EOT
)

(stfl:init!)

(define form (stfl:create layout))

(define vcard-template #<<EOVCARD
BEGIN:VCARD
VERSION:2.1
FN:~a ~a
N:~a;~a
EMAIL;INTERNET:~a
END:VCARD
EOVCARD
)

(define (save-vcard!)
  (let ((filename (stfl:get-value form "filetext")))
    (when filename
      (let ((first-name (stfl:get-value form "firstnametext"))
            (last-name (stfl:get-value form "lastnametext"))
            (email (stfl:get-value form "emailtext")))
        (with-output-to-file filename
          (lambda ()
            (display (format vcard-template
                             first-name last-name
                             last-name first-name
                             email)))))
      (stfl:set-value! form "msg" (format "Stored vCard to ~a" filename)))))

(let loop ()
  (let ((event (stfl:run! form 0))
        (focus (stfl:get-focus form)))
    (cond
     ((equal? event "ESC") #f)
     ((and (equal? event "ENTER")
           (equal? focus "file"))
      (stfl:reset!)
      (save-vcard!)
      (loop))
     (else (loop)))))
