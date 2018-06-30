;; -*- no-byte-compile: t; -*-
;;; private/lsp/packages.el

(when (package! lsp-mode)
  (package! lsp-ui)
  (package! company-lsp))
