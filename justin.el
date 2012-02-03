;; Set initial layout
(setq default-frame-alist
      '((top . 2) (left . 22) (width . 129) (height . 50)))

;; Keybinding for commenting region
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; This is already enabled with M-; by default

;; Keybinding for ido find-file-at-point
(global-set-key (kbd "C-x a") 'find-file-at-point)

;; rhtml mode https://github.com/eschulte/rhtml.git
(add-to-list 'load-path "~/.emacs.d/personal/rhtml")
(require 'rhtml-mode)
;; rvm.el should allow us to automatically load the correct ruby by
;; looking at the associated .rvmrc
(require 'rvm)
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

;; Tramp Stuff
(setq tramp-default-port 2211)

;; SBCL + SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(add-to-list 'load-path "/home/justin/quicklisp/dists/quicklisp/software/slime-20111105-cvs")
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-fuzzy))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Set text to 10pt
(set-face-attribute 'default nil :height 100)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

;; Add expand region
    (require 'expand-region)
    (global-set-key (kbd "C-@") 'er/expand-region)

;; REMINDERS
;;
;; 'M-x cua-mode' enables rectangular text selection/editing DONT
;; FORGET IT
;;
;; M-/ hippie-expand
;; it will look for completions in the current buffer, other open
;; buffers, and the kill ring
;;
;; M-x follow-mode open a long file, then two buffers, enabel follow
;; mode to stretch the file across two or more buffers, C-x + to
;; balance the windows, very cool
