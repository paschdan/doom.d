;;;  -*- lexical-binding: t; -*-

;; maximize
(toggle-frame-maximized)

;; user settings
(setq
 user-full-name "Daniel Paschke"
 user-mail-address "paschdan@gmail.com"
 doom-theme 'doom-vibrant)

(setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 18))

;; snippets dir
(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(add-hook! clojure-mode 'rainbow-delimiters-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

(def-package! zoom-frm
  :commands (zoom-frm-out zoom-frm-in))

(map!
      :ne "M-="   #'zoom-frm-in
      :ne "M--"   #'zoom-frm-out)
