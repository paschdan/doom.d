;;;  -*- lexical-binding: t; -*-

;; maximize
(toggle-frame-maximized)

;; user settings
(setq
 user-full-name "Daniel Paschke"
 user-mail-address "paschdan@gmail.com"
 doom-theme 'doom-vibrant)

;; snippets dir
(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(add-hook! clojure-mode 'rainbow-delimiters-mode)
(add-hook! emacs-lisp-mode 'smartparens-strict-mode)


;; (def-package! lispyville
;;  :after evil
;;  :commands lispyville-mode
;;  :init
;;  (add-hook! emacs-lisp-mode 'lispyville-mode)
;;  (add-hook! clojure-mode 'lispyville-mode)
;;  :config
;;  (lispyville-set-key-theme
;;   '(operators
;;     )))

;; (def-package! evil-lispy
;;   :after evil
;;   :commands evil-lispy-mode
;;   :init
;;   (add-hook! emacs-lisp-mode 'evil-lispy-mode))

(def-package! evil-cleverparens
  :commands evil-cleverparens-mode
  :init
    (setq  evil-cleverparens-use-additional-bindings nil)
    (add-hook! emacs-lisp-mode #'evil-cleverparens-mode))
