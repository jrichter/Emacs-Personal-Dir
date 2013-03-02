(setq user-full-name "Justin Richter")
(setq user-login-name "justin")
(setq user-mail-address "jrichter@jetfive.com")

;; Load packages not installed
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      nrepl
                      ac-nrepl
                      color-theme-sanityinc-tomorrow
                      color-theme-sanityinc-solarized
                      fill-column-indicator
                      auto-complete
                      undo-tree
                      diminish
                      js2-mode
                      js2-refactor
                      lua-mode
                      gnus
                      bbdb
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;; Load a custom theme
(load-theme 'sanityinc-tomorrow-eighties t)

;; Set Google Chrome as default
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

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
                           c-mode c++-mode objc-mode ruby-mode slim-mode lua-mode clojure-mode
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


;; Some usefull keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; auto-complete mode
(require 'auto-complete-config)

;; ace-jump-mode
(global-set-key (kbd "C-c ;") 'ace-jump-mode)
(global-set-key (kbd "C-c :") 'ace-jump-word-mode)

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

;; Rename File/Buffer
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Delete File for real
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

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
;; (add-to-list 'load-path "~/.emacs.d/personal/lua-mode")
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

;; ;; SBCL + SLIME
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; ;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
;; (add-to-list 'load-path "/home/justin/quicklisp/dists/quicklisp/software/slime-20111105-cvs")
;; (require 'slime-autoloads)
;; (slime-setup '(slime-fancy slime-fuzzy))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Clojure Mode
(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . clojure-mode))
(add-hook 'clojure-mode-hook
          'clojure-test-mode)

;; Clojure nrepl stuff
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;;You can control the TAB key behavior in the REPL via the nrepl-tab-command variable.
;;While the default command nrepl-indent-and-complete-symbol should be an adequate choice
;;for most users, it's very easy to switch to another command if you wish to. For instance
;;if you'd like TAB to only indent (maybe because you're used to completing with M-TAB)
;;use the following snippet:
(setq nrepl-tab-command 'indent-for-tab-command)

;;Stop the error buffer from popping up while working in the REPL buffer:
(setq nrepl-popup-stacktraces nil)

;;Make C-c C-z switch to the *nrepl* buffer in the current window:
(add-to-list 'same-window-buffer-names "*nrepl*")

;;Enabling CamelCase support for editing commands(like forward-word, backward-word, etc)
;;in nREPL is quite useful since we often have to deal with Java class and method names.
;;The built-in Emacs minor mode subword-mode provides such functionality:
(add-hook 'nrepl-mode-hook 'subword-mode)

;;The use of paredit when editing Clojure (or any other Lisp) code is highly recommended.
;;You're probably using it already in your clojure-mode buffers (if you're not you
;;probably should). You might also want to enable paredit in the nREPL buffer as well:
(add-hook 'nrepl-mode-hook 'paredit-mode)

;;RainbowDelimiters is a minor mode which highlights parentheses, brackets, and braces
;;according to their depth. Each successive level is highlighted in a different color.
;;This makes it easy to spot matching delimiters, orient yourself in the code, and tell
;;which statements are at a given depth. Assuming you've already installed
;;RainbowDelimiters you can enable it in nREPL like this:
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;;fix for ac-nrepl
(setq nrepl-connected-hook (reverse nrepl-connected-hook))

;; W3M
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
(require 'w3m-load)

;; Helm Mode
(helm-mode 1)

;; Disable Projectile Mode as it is sloooow on the cr48
;; (projectile-global-mode -1)

;; Set text to 12pt
(set-face-attribute 'default nil :height 100)

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
(add-hook 'clojure-mode-hook 'fci-mode)

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


;; Diminish modeline clutter
(require 'diminish)
(diminish 'helm-mode)
(diminish 'projectile-mode)
(diminish 'prelude-mode)
(diminish 'yas-minor-mode)
(add-hook 'ruby-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'js-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'js2-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'html-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'css-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'scss-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'sh-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'lisp-mode-hook (lambda () (diminish 'guru-mode)))
(add-hook 'ruby-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'js-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'js2-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'html-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'css-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'scss-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'sh-mode-hook (lambda () (diminish 'volatile-highlights-mode)))
(add-hook 'lisp-mode-hook (lambda () (diminish 'volatile-highlights-mode)))

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
