;; Allow search in gmail from gnus
;; With G G in the Groups buffer, you search for mails in the current group. Note that this not work in virtual groups. If you want to search on all your mails, you should add the folder ‘All Mail’.
(require 'nnir)

;; bind gnus to F11
;;(global-set-key [f11] 'gnus)

;; Webjump
(global-set-key (kbd "C-x g") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites
                '("Urban Dictionary" .
                  [simple-query
                   "www.urbandictionary.com"
                   "http://www.urbandictionary.com/define.php?term="
                   ""])))

(defun launch_gnus_new_frame ()
  "Open a new frame and then launch gnus"
  (interactive)
  (let ((gmail_frame (make-frame '((name . "gmail") (window-system . x)))))
    (select-frame-set-input-focus gmail_frame)
    (if window-system
        (set-frame-size (selected-frame) 101 90))
    (gnus)
    )
  )

(global-set-key [f11] 'launch_gnus_new_frame)

(provide 'custom-gnus)
