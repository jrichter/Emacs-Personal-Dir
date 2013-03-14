;; Set initial layout
(setq default-frame-alist
      '((width . 101) (height . 90) (left-fringe . 8) (right-fringe . 4)))

(fringe-mode (cons 8 4))
;; Load a custom theme
;;(load-theme 'sanityinc-tomorrow-eighties t)

;; Change theme based on time of day
(defun synchronize-theme ()
  (setq hour
        (string-to-number
         (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 17))
      (load-theme 'solarized-light t)
    (load-theme 'sanityinc-tomorrow-eighties t))
  )

(run-with-timer 0 3600 'synchronize-theme)

;; Set text to 12pt except on jet which has a lower resolution
(if (string= system-name "jet")
    (set-face-attribute 'default nil :height 100)
  (set-face-attribute 'default nil :height 120))

;; Set whitespace long line to really far off
(setq whitespace-line-column 250)

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

(provide 'my-layout-and-theme)
