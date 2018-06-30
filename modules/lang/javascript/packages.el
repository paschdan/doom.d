;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; requires node npm js-beautify eslint eslint-plugin-react

(package! coffee-mode)
(package! eslintd-fix)
(package! js2-mode)
(package! js2-refactor)
(package! nodejs-repl)
(package! rjsx-mode)
(package! skewer-mode)
(package! typescript-mode)
(package! web-beautify)
(package! lsp-javascript-typescript
      :recipe (:fetcher
               github
               :repo "emacs-lsp/lsp-javascript"
               :files ("lsp-javascript-typescript.el")))

(when (featurep! :feature lookup)
  (package! xref-js2))

