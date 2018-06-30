;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! (:any js2-mode web-mode)
  (set-pretty-symbols! '(js2-mode web-mode)
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true" :false "false"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    ;; Other
    :yield "import"))


;;
;; Major modes
;;

(def-package! js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js-chain-indent t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil)

  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  ;; Indent switch-case another step
  (setq-hook! 'js2-mode-hook js-switch-indent-offset js2-basic-offset)

  (set-electric! 'js2-mode :chars '(?\} ?\) ?. ?:))
  (set-repl-handler! 'js2-mode #'+javascript/repl)

  (map! :map js2-mode-map
        :localleader
        :n "S" #'+javascript/skewer-this-buffer))


(def-package! rjsx-mode
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  (set-electric! 'rjsx-mode :chars '(?\} ?\) ?. ?>))
  (add-hook! 'rjsx-mode-hook
    ;; jshint doesn't know how to deal with jsx
    (push 'javascript-jshint flycheck-disabled-checkers))

  ;; `rjsx-electric-gt' relies on js2's parser to tell it when the cursor is in
  ;; a self-closing tag, so that it can insert a matching ending tag at point.
  ;; However, the parser doesn't run immediately, so a fast typist can outrun
  ;; it, causing tags to stay unclosed, so force it to parse.
  (defun +javascript|reparse (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    (if (= n 1) (rjsx-maybe-reparse)))
  (advice-add #'rjsx-electric-gt :before #'+javascript|reparse))


(after! typescript-mode
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break)
  (set-electric! 'typescript-mode
    :chars '(?\} ?\)) :words '("||" "&&"))
  (set-pretty-symbols! 'typescript-mode
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "number"
    :str "string"
    :bool "boolean"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return" :yield "import"))


;; `coffee-mode'
(setq coffee-indent-like-python-mode t)


;;
;; Tools
;;

;; lsp
(def-package! lsp-javascript-typescript
  :config
  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable))


(def-package! xref-js2
  :when (featurep! :feature lookup)
  :commands xref-js2-xref-backend
  :init (set-lookup-handlers! 'js2-mode :xref-backend #'xref-js2-xref-backend))


(def-package! js2-refactor
  :commands
  (js2r-extract-function js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf))


(def-package! eslintd-fix
  :commands eslintd-fix
  :config
  (defun +javascript|set-flycheck-executable-to-eslint ()
    (setq flycheck-javascript-eslint-executable eslintd-fix-executable))
  (add-hook 'eslintd-fix-mode-hook #'+javascript|set-flycheck-executable-to-eslint))


;; `skewer-mode'
(map! (:after skewer-mode
        :map skewer-mode-map
        :localleader
        :n "sE" #'skewer-eval-last-expression
        :n "se" #'skewer-eval-defun
        :n "sf" #'skewer-load-buffer)

      (:after skewer-css
        :map skewer-css-mode-map
        :localleader
        :n "se" #'skewer-css-eval-current-declaration
        :n "sr" #'skewer-css-eval-current-rule
        :n "sb" #'skewer-css-eval-buffer
        :n "sc" #'skewer-css-clear-all)

      (:after skewer-html
        :map skewer-html-mode-map
        :localleader
        :n "se" #'skewer-html-eval-tag))


;; `web-beautify'
(map! :map* (json-mode-map js2-mode-map) :n "gQ" #'web-beautify-js)


;;
;; Projects
;;

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(?:-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! "+screeps"))

(def-project-mode! +javascript-gulp-mode
  :files ("gulpfile.js"))

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files ("package.json")
  :add-hooks (+javascript|add-node-modules-path))

