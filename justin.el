(setq user-full-name "Justin Richter")
(setq user-login-name "justin")
(setq user-mail-address "jrichter@jetfive.com")

;; Load a custom theme
(load-theme 'sanityinc-tomorrow-eighties t)

;; Make compilation window not steal a buffer
(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window)
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)))

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; load some js2-mode defaults from magnars
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'html-mode '(require 'setup-html-mode))

;; sh-mode stuff
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . sh-mode))

;; When splitting a buffer move point to new buffer
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; After yank, indent region
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode sh-mode js-mode js2-mode
                           c-mode c++-mode objc-mode ruby-mode slim-mode lua-mode
                           LaTeX-mode TeX-mode html-mode scss-mode css-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Set initial layout
(setq default-frame-alist
      '((width . 101) (height . 90)))

;; Keybinding for commenting region
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; This is already enabled with M-; by default

;; terminal stuff
(global-set-key (kbd "C-x t") '(lambda ()(interactive)(ansi-term "/bin/zsh")))

;; Keybinding for ido find-file-at-point
(global-set-key (kbd "C-x a") 'find-file-at-point)


;; Some usefule keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; auto-complete mode
(require 'auto-complete-config)

;; ace-jump-mode
(global-set-key (kbd "C-c ;") 'ace-jump-mode)

;; Buffer related from Magnars github
(require 'imenu)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)
(global-unset-key (kbd "C-x C--"))
(global-set-key (kbd "C-x C--") 'toggle-window-split)


;; rhtml mode https://github.com/eschulte/rhtml.git
(add-to-list 'load-path "~/.emacs.d/personal/rhtml")
(require 'rhtml-mode)

;; ruby-mode tools
(add-to-list 'load-path "~/.emacs.d/personal/ruby-tools")
(require 'ruby-tools)

;; slim-mode
(add-to-list 'load-path "~/.emacs.d/personal/")
(require 'slim-mode)

;; lua-mode
(add-to-list 'load-path "~/.emacs.d/personal/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


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

;; W3M
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
(require 'w3m-load)

;; Helm Mode
(helm-mode 1)

;; Disable Projectile Mode as it is sloooow on the cr48
;; (projectile-global-mode -1)

;; Set text to 12pt
(set-face-attribute 'default nil :height 120)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Set whitespace long line to really far off
(setq whitespace-line-column 250)

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

;; Make SCSS mode not compile at file save
(setq scss-compile-at-save nil)

;;I think prelude uses this now ;; Add expand region
;; (add-to-list 'load-path "~/.emacs.d/personal/er")
;;     (require 'expand-region)
;;     (global-set-key (kbd "C-@") 'er/expand-region)

;;Make yasnippet have the correct keybinding when editing C files
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/personal/snippets"))
(yas-global-mode 1)
( global-set-key [f7] 'yas/insert-snippet)

;;Add yari Yet Another RI...
(add-to-list 'load-path "~/.emacs.d/personal/yari")
(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f6] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)

;; TRAMP custom stuff
(add-to-list 'tramp-default-method-alist '("home" "" "scp"))
(add-to-list 'tramp-default-method-alist '("5.jetfive.com" "" "scp"))

;; Auto complete mode
(require 'auto-complete)
(add-to-list 'ac-modes 'ruby-mode 'javascript-mode)
(setq ac-sources '(ac-source-semantic ac-source-yasnippet))
(global-auto-complete-mode)

;; Show line at 90 char
(require 'fill-column-indicator)
(setq-default fill-column 90)
(setq-default fci-rule-width 1)
(setq-default fci-rule-color "#686868")
(add-hook 'ruby-mode-hook 'fci-mode)
(add-hook 'js-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))


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
;;
;; C-h m show all active minor and major mode key bindings
;; C-c C-e sgml-close-tag
;; C-c C-w html-mode wrap-tag which is a custom definition



;; Multiple Cursors
(add-to-list 'load-path "~/.emacs.d/personal/multiple-cursors")
(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c >") 'mc/mark-sgml-tag-pair)
;; From active region to multiple cursors:
;; (global-set-key (kbd "C-M-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-M-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-M-c C-a") 'mc/edit-beginnings-of-lines)

;; Define Custom Combos

(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(defun quick-add-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (kill-append (buffer-substring beg end) (< end beg)))
  (beginning-of-line 2))


( global-set-key [f8] 'quick-copy-line)
( global-set-key [f9] 'quick-add-line)


;; kill flyspell-mode
(defun fix-prelude-prog-mode-defaults ()
  (turn-off-flyspell))

(add-hook 'prelude-prog-mode-hook 'fix-prelude-prog-mode-defaults t)
