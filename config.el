;;;  -*- lexical-binding: t; -*-


;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(after! js2-mode
  ;; use eslintd-fix so when i save it fixes dumb shit
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(after! rjsx-mode
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(defun my/my-proj-relative-buf-name ()
     (file-relative-name (or buffer-file-name (bound-and-true-p list-buffers-directory)) (projectile-project-root)))

(defun my/yank-buffer-filename-relative-to-project ()
  (interactive)
  (message (kill-new(paschdan/my-proj-relative-buf-name))))

;; ==================================================
;; Settings
;; ==================================================

;; maximize
(toggle-frame-maximized)

;; user settings
(setq
 user-full-name "Daniel Paschke"
 user-mail-address "paschdan@gmail.com"
 doom-theme 'doom-vibrant)

;; UI
(setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 18))
(setq doom-vibrant-brighter-modeline t)

;; snippets dir
(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(add-hook! clojure-mode 'rainbow-delimiters-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

(def-package! zoom-frm
  :commands (zoom-frm-out zoom-frm-in))


;; ==================================================
;; Keybindings
;; ==================================================

(map!
      :n "M-="   #'zoom-frm-in
      :n "M--"   #'zoom-frm-out)

(map! :leader
        :desc "Yank filename relative to proj"             :n "y" #'my/yank-buffer-filename-relative-to-project)
