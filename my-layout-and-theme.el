;; Set initial layout
(setq default-frame-alist
      '((width . 101) (height . 90)))

;; Load a custom theme
(load-theme 'sanityinc-tomorrow-eighties t)

;; Set text to 12pt
(set-face-attribute 'default nil :height 120)

;; Set whitespace long line to really far off
(setq whitespace-line-column 250)

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

(provide 'my-layout-and-theme)
