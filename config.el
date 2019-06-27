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
  (message (kill-new(my/my-proj-relative-buf-name))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
;; ==================================================
;; Settings
;; ==================================================


;; user settings
(setq
 user-full-name "Daniel Paschke"
 user-mail-address "paschdan@gmail.com"
 doom-theme 'doom-nord)

;; UI
(setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 18))
(setq doom-vibrant-brighter-modeline t)

;; snippets dir
(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(add-hook! clojure-mode 'rainbow-delimiters-mode)

;; ==================================================
;; Packages
;; ==================================================

(def-package! zoom-frm
  :commands (zoom-frm-out zoom-frm-in))

;; git-link
(use-package git-link)

;; ==================================================
;; Keybindings
;; ==================================================

(map!
      :n "M-="   #'zoom-frm-in
      :n "M--"   #'zoom-frm-out

      (:after evil-magit
        :map (magit-mode-map)
        :n "yc" #'git-link-commit)
      )

(map! :leader
        :desc "Yank filename relative to proj"             :n "y" #'my/yank-buffer-filename-relative-to-project
        :desc "Copy git url" :n "gu" #'git-link
        :desc "Rename current file" :n "fr" #'rename-current-buffer-file
        :desc "resume latest ivy" :nv "=" #'ivy-resume)

;; maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))
