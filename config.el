;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Scott Trinh"
      user-mail-address "scott@edgedb.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun evil-surround-type ()
  "Read a typename from the minibuffer and wrap selection in angle brackets"
  (let ((tname (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s<" (or tname ""))
          ">")))

(defun prettier/region (start end)
  "Format the region from START to END using prettier with a selected parser."
  (interactive "r")  ; Prompt the user for the region.
  (let* ((parsers '("typescript" "babel" "css" "html" "json" "yaml"))
         (parser (completing-read "Select parser: " parsers))
         (command (concat "prettier --parser " parser)))
    (shell-command-on-region start end command nil t)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-subword-mode 1)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'js2-mode-hook +format-with-lsp nil)
(setq-hook! 'js-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook
  web-mode-code-indent-offset 2
  web-mode-markup-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-attr-indent-offset 2)
(setq-hook! 'typescript-mode-hook typescript-indent-level 2)
(after! org-tree-slide
  (defadvice! +org-present--hide-first-heading-maybe-a (fn &rest args)
    "Omit the first heading if `+org-present-hide-first-heading' is non-nil."
    :around #'org-tree-slide--display-tree-with-narrow
    (letf!
      (defun org-narrow-to-subtree (&optional element)
        "Narrow buffer to the current subtree."
        (interactive)
        (save-excursion
          (save-match-data
            (org-with-limited-levels
             (narrow-to-region
              (progn
                (when (org-before-first-heading-p)
                  (org-next-visible-heading 1))
                (org-back-to-heading t)
                (when +org-present-hide-first-heading
                  (forward-line 1))
                (point))
              (progn
                (org-end-of-subtree t t)
                (when (and (org-at-heading-p) (not (eobp)))
                  (backward-char 1))
                (point)))))))
      (apply fn args))))
(setq +org-present-text-scale 3)
(setq +word-wrap-fill-style 'soft)
(setq company-idle-delay nil)

(after! evil-surround
  (let ((pairs '((?T . evil-surround-type))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

(after! flycheck
  (map! :leader
        :desc "Errors" "e" flycheck-command-map))
(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'light))
(set-formatter! 'alejandra "alejandra-quiet" :modes '(nix-mode))
(setq-hook! 'nix-mode-hook nix-nixfmt-bin "alejandra-quiet")
(setq format-all--executable-table (let ((table (make-hash-table))) (puthash 'nixfmt "alejandra-quiet" table) table))

(defvar esdl-mode-syntax-table nil
  "Syntax table for `esdl-mode'.")

(setq esdl-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; python style comment: "# …"
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

(setq esdl-font-lock-keywords
      (let* (
             ;; Keywords
             (x-keywords-regexp "\\(module\\|abstract type\\|type\\|constraint\\|exclusive\\|rewrite\\|trigger\\|extending\\|required\\|multi\\|single\\|property\\|link\\|scalar\\)")
             ;; Scalar types
             (x-scalar-types-regexp "\\(str\\|bool\\|int16\\|int32\\|int64\\|float32\\|float64\\|bigint\\|decimal\\|json\\|uuid\\|bytes\\|datetime\\|duration\\)")
             ;; Properties in an attribute, e.g. `fields: [MediaTypeId]'.
             (x-properties-regexp "[a-zA-Z_-]+:")
             ;; Builtin functions. E.g. `autoincrement()'
             (x-attribute-functions-regexp "\\([a-z_]\\)+\(\.*\)")
             ;; Constants
             (x-constants-regexp "\\(true\\|false\\)")
             ;; Custom types
             (x-custom-type-regexp "[A-Z]+[a-zA-Z]+")
             )
        `(
          ;; order matters
          (,x-attribute-functions-regexp . (1 font-lock-function-name-face))
          (,x-keywords-regexp . (1 font-lock-keyword-face))
          (,x-scalar-types-regexp . (1 font-lock-type-face))
          (,x-properties-regexp . font-lock-variable-name-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-custom-type-regexp . font-lock-type-face)
          )))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STRT(s)" "REVIEW(r)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  (setq org-hierarchical-todo-statistics nil)
  (setq org-agenda-files '("~/org/" "~/org/roam/" "~/org/roam/daily/" "~/org/roam/weekly/"))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "** %<%Y-%m-%d %a>
*** Daily Planning
**** [[id:52f49aaf-f14c-44d4-9404-3235a86af30b][Most important tasks]]
***** Personal
***** EdgeDB
****** [[id:171b41ef-c2f9-43e0-8583-6324acea883a][DevRel]]
****** [[id:2ca2f930-6695-40ef-8d42-653fff9a0390][TypeScript Core]]
****** [[id:a3c793da-973a-4f9f-87d6-01765bb2ca63][Auth]]
*** Journal
*** Habits
**** TODO [[id:c6621398-737d-412f-a0a6-421a847b38c5][Object writing]]
- Prompt: %?
**** TODO [[id:8502be12-42cb-4f3a-bfb5-e8b3327b0548][Lazy dad workout]]
- Four count:
- SEALs:
**** TODO [[id:c98bf5ad-6e1b-457e-ba41-f83acf5cd000][Starting stretching]]
*** Daily Review
**** Progress on projects
**** Challenges
**** Dear tomorrow
"
           ;;"* %<%Y-%m-%d %a> %?"
           :target (file+head "~/org/roam/weekly/%<%Yw%V>.org"
                              "#+title: %<%Yw%V>\n
* Weekly Planning
** Big Rocks
** Main Agenda Items
** Short-term Goal


* Weekly Review
** Get Clear
*** TODO Process all inboxes
*** TODO Brain dump
** Get Current
*** TODO Review long-term and short-term goal
*** TODO Review the calendar
*** TODO Review actions lists
- [ ] Project lists
- [ ] Someday/maybe

")))))

(define-derived-mode esdl-mode js2-mode "ESDL"
  "EdgeDB SDL major mode."
  :syntax-table esdl-mode-syntax-table

  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq c-syntactic-indentation nil)
  (setq js-indent-level 2)
  (setq-local comment-start "#")

  ;; HACK: dont indent after <type>[?!]
  (setq-local js--indent-operator-re "")
  (setq font-lock-defaults '((esdl-font-lock-keywords)))
  ;; disable syntax checking
  (setq-local js2-mode-show-parse-errors nil)
  (setq-local js2-mode-show-strict-warnings nil))

(add-to-list 'auto-mode-alist '("\\.esdl\\'" . esdl-mode))

;; accept completion from copilot and fallback to company
;;(use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (:map copilot-completion-map
;;              ("C-f" . 'copilot-accept-completion)
;;              ("C-g" . 'copilot-clear-overlay)))

(use-package! org-ai
  :hook (org-mode . org-ai-mode))
