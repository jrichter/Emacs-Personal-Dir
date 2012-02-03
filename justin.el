;; Set initial layout
(setq default-frame-alist
      '((top . 2) (left . 22) (width . 129) (height . 50)))

;; Keybinding for commenting region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; rhtml mode https://github.com/eschulte/rhtml.git
(add-to-list 'load-path "~/.emacs.d/personal/rhtml")
(require 'rhtml-mode)

;; Set text to 10pt
(set-face-attribute 'default nil :height 100)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; (setq whitespace-newline 'whitespace-newline)
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))
