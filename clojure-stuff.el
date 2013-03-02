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

(provide 'clojure-stuff)
