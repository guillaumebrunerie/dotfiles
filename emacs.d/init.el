;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure the use-package macro to work with straight
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

;;;;;;;;;;;;;
;; Various ;;
;;;;;;;;;;;;;

; No welcome message
(setq inhibit-startup-message t)

; Cleaner interface
(tool-bar-mode -1)
(menu-bar-mode -1)

; Visible bell
(setq visible-bell t)

; Customize window title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

; Column number
(column-number-mode 1)

; Auto-fill by default in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Recherche automatique des fermetures et ouvertures des parenthèses
;(load-library "paren")
(show-paren-mode 1)

; Pour que la sélection soit remplacée par ce que l'on tape
(pending-delete-mode)

; Eviter que la cesure de fin de ligne, operée par exemple par le  mode autofill ou par un M-q, coupe au niveau d'un caractere ( ou :
(add-hook 'fill-no-break-predicate 'fill-french-nobreak-p)

; M-g pour aller à la x-ième ligne
;(global-set-key [(meta g)] 'goto-line)

; ----------------------------------------------------------------------
; Divers

(setq confirm-kill-emacs 'yes-or-no-p)

; No backup files
(setq make-backup-files nil)

; Pour ne pas avoir à taper en entier la réponse yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; Affichage des images et fichiers compressés
(setq auto-image-file-mode t)
(setq auto-compression-mode t)

; Molette de la souris
(mouse-wheel-mode 1)

; No annoying lockfiles
(setq create-lockfiles nil)

;; History for minibuffer
(savehist-mode)

;; Deal with long lines
(global-so-long-mode 1)

;;;;;;;;;;;
;; OCaml ;;
;;;;;;;;;;;

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger." t)
(setq-default auto-mode-alist
              (cons '("\\.ml[iylp]?" . tuareg-mode)
                    auto-mode-alist))
(setq-default auto-mode-alist
              (cons '("\\.prehtml" . html-mode)
                    auto-mode-alist))
(setq-default auto-mode-alist
              (cons '("\\.mhtml" . html-mode)
                    auto-mode-alist))
(setq-default tuareg-in-indent 0)
(defvar caml-mode-hook
  (lambda () (modify-syntax-entry ?_ "w" caml-mode-syntax-table)))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (define-key tuareg-mode-map "\M-q" 'fill-paragraph)))

(add-hook 'tuareg-load-hook
          (lambda ()
            (define-key tuareg-mode-map [f9] 'tuareg-eval-buffer)
            (define-key tuareg-mode-map [f10] 'tuareg-eval-phrase)
            (outline-minor-mode)
            (define-key tuareg-mode-map [M-left] `hide-subtree)
            (define-key tuareg-mode-map [M-right] `show-subtree)))

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook (lambda ()
                             (LaTeX-math-mode 1)
                             (global-linum-mode t)
                             (TeX-PDF-mode 1)
                             (TeX-fold-mode 1)
                             (setq TeX-parse-self t)
                             (setq TeX-auto-save t)
                             (set-fill-column 100)
                             )
          )
(ignore-errors (load "auctex.el" nil t t))
(ignore-errors (load "preview-latex.el" nil t t))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq ispell-tex-skip-alists
                  (list
                   (append
                    (car ispell-tex-skip-alists)
                    '(("[^\\]\\$" . "[^\\]\\$") ("\\[" . "\\]") ("\\\\operatorname" ispell-tex-arg-end)))
                   (append
                    (cadr ispell-tex-skip-alists)
                    '(("align\\*?" . "\\\\end{align\\*?}")))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal emulator ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar vterm-background-cookies nil)
(defun toggle-vterm-background ()
  "Set up a different background color if we are in vterm mode but not in vterm copy mode"
  (if (and (string= major-mode "vterm-mode") (not vterm-copy-mode))
	  (setq vterm-background-cookies (cons (face-remap-add-relative 'default 'vterm-background) (face-remap-add-relative 'fringe 'vterm-background)))
	(face-remap-remove-relative (car vterm-background-cookies))
	(face-remap-remove-relative (cdr vterm-background-cookies))))

(use-package vterm
  ;; :custom
  ;; (vterm-buffer-name-string "%s [vterm]")
  :bind
  (:map vterm-mode-map
		("<mouse-4>" . vterm-send-up)
		("<wheel-up>" . vterm-send-up)
		("<mouse-5>" . vterm-send-down)
		("<wheel-down>" . vterm-send-down))
  :hook
  (vterm-mode . (lambda () (text-scale-adjust -1)))
  (vterm-mode . toggle-vterm-background)
  (vterm-copy-mode . toggle-vterm-background))

(use-package multi-vterm
  :bind
  (("C-c t" . multi-vterm-project) ))

;; If we aren’t in any project, make multi-vterm believe we're in a project at ~
(advice-add 'multi-vterm-project-root :filter-return (lambda (r) (or r "~")))

(use-package shell-script-mode
  :straight nil
  :mode "\\(\\.sh\\'\\|zshrc\\)")

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit)

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (push "makefile" projectile-project-root-files-bottom-up)
  (push "go.mod" projectile-project-root-files-bottom-up)
  (push "package.json" projectile-project-root-files-bottom-up))

;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(use-package phindent-mode
  :straight (:host github :repo "guillaumebrunerie/phindent-mode"))

(defun infer-indentation-style ()
  ;; If our source file uses tabs somewhere, or doesn’t but doesn’t use spaces
  ;; either (for instance an empty file), we use tabs, otherwise we use spaces.
  (setq-local indent-tabs-mode (or (> (how-many "^\t" (point-min) (point-max)) 0)
                                   (= (how-many "^    " (point-min) (point-max)) 0))))

(defun infer-indentation-amount ()
  (let ((two-indented-lines (how-many "^  [^ ]" (point-min) (point-max)))
		(tab-indented-lines (how-many "^\t" (point-min) (point-max))))
    (if (and (> two-indented-lines 2) (= tab-indented-lines 0))
		(setq-local js-indent-level 2)
	  (setq-local js-indent-level 4))))

(use-package js2-mode
  :hook
  (js2-mode . infer-indentation-style)
  (js2-mode . infer-indentation-amount)
  (js2-mode . phindent-mode)
  (js2-mode . whitespace-mode)
  :config
  (setq js-switch-indent-offset js-indent-level))
(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))
(use-package js2-highlight-vars
  :hook (js2-mode . js2-highlight-vars-mode))

(use-package rjsx-mode
  :hook
  (rjsx-mode . infer-indentation-style)
  (rjsx-mode . phindent-mode)
  (rjsx-mode . whitespace-mode))

(use-package ultimate-js-mode
  :straight (:host github :repo "guillaumebrunerie/ultimate-js-mode" :files (:defaults "libs" "queries"))
  :mode ("\\.[jt]sx?\\'" "\\.json\\'")
  :hook
  (ultimate-js-mode . infer-indentation-style)
  (ultimate-js-mode . infer-indentation-amount)
  (ultimate-js-mode . phindent-mode)
  (ultimate-js-mode . whitespace-mode)
  (ultimate-js-mode . lsp-deferred)
  ;; (ultimate-js-mode . eglot-ensure)
  ;; (ultimate-js-mode . (lambda () (flymake-eslint-enable)))
  :config
  (setq js-switch-indent-offset js-indent-level))

;;;;;;;;;;;;
;; Prisma ;;
;;;;;;;;;;;;

(use-package emacs-prisma-mode
  :straight (:host github :repo "pimeys/emacs-prisma-mode")
  :mode "\\.prisma\\'")

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;; If we choosed a candidate, insert it, otherwise do the normal tab-completion
(defun my-tab-complete ()
  (interactive)
  (if (= vertico--index -1) (minibuffer-complete) (vertico-insert)))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-preselect 'first)
  :bind
  (:map vertico-map ("TAB" . #'my-tab-complete)))

(use-package vertico-prescient
  :config
  (vertico-prescient-mode 1)
  (setq prescient-filter-method '(regexp fuzzy))
  (prescient-persist-mode 1))

(use-package marginalia
  :config
  (marginalia-mode +1))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match t)
  :bind (("s-<tab>" . #'completion-at-point)
		 :map corfu-map
			  ([remap move-beginning-of-line] . nil)
			  ([remap move-end-of-line] . nil)
			  ([remap beginning-of-buffer] . nil)
			  ([remap end-of-buffer] . nil)
			  ([remap scroll-down-command] . nil)
			  ([remap scroll-up-command] . nil)
			  ([remap next-line] . nil)
			  ([remap previous-line] . nil)
			  ([remap keyboard-escape-quit] . corfu-quit)
			  ("C-a" . nil)
			  ("M-n" . nil)
			  ("M-p" . nil))
  :init
  (global-corfu-mode))

(use-package corfu-prescient)

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package company
;;   :hook
;;   (after-init . global-company-mode)
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.2))

;; (use-package company-prescient
;;   :config
;;   (company-prescient-mode +1))

;; Case insensitive fuzzy completion in buffer
(add-to-list 'completion-styles 'flex)
(setq completion-ignore-case t)

;;;;;;;;;;;;;;;;;;
;; Code folding ;;
;;;;;;;;;;;;;;;;;;

(defun my/hs-is-folded ()
  "Returns non-nil if the current line is folded, and returns the end position of the folding"
  (let* ((overlays-on-line (append (overlays-in (point-at-eol) (+ (point-at-eol) 1)) (overlays-in (1- (point-at-bol)) (point-at-bol)))))
	(seq-some (lambda (o) (and (overlay-get o 'hs) (overlay-end o))) overlays-on-line)))

(defun my/hs-toggle (arg)
  "If the current line is folded, unfold only one level of it, otherwise fold it."
  (interactive "p")
  (if-let ((fold-position (my/hs-is-folded)))
	  (save-excursion
		(goto-char fold-position)
		(hs-hide-level arg))
	(save-excursion
	  (move-end-of-line arg)
	  (hs-hide-block))))

(defun my/hs-open (arg)
  "Unfold entirely the current line"
  (interactive "p")
  (save-excursion
	(if (> arg 1)
		(hs-show-all)
	  (if-let ((fold-position (my/hs-is-folded)))
		  (progn
			(goto-char fold-position)
			(hs-show-block))
		(move-beginning-of-line arg)
		(hs-show-block)))))

(defun my/hs-close (arg)
  (interactive "p")
  (save-excursion
	(if (> arg 1)
		(hs-hide-all)
	  (move-end-of-line arg)
	  (hs-hide-block))))

(defface hs-folded '((t :background "#782200" :foreground "#737373")) "Dots")

(use-package hs-minor-mode
  :straight nil
  :bind
  (("C-<tab>" . my/hs-toggle)
   ("<backtab>" . my/hs-close)
   ("C-S-<tab>" . my/hs-open)
   ("<C-iso-lefttab>" . my/hs-open))
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :config
  (hs-minor-mode)
  (setq hs-set-up-overlay (lambda (o) (overlay-put o 'display (propertize "[···]" 'face 'hs-folded)))))

(use-package prog-mode
  :straight nil
  :hook (prog-mode . hs-minor-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Language Server ;;
;;;;;;;;;;;;;;;;;;;;;

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(ultimate-js-mode . ("typescript-language-server" "--stdio"))))

;; (use-package flymake-eslint)

;; (use-package flymake
;;   :bind
;;   (("M-<down>" . flymake-goto-next-error)
;;    ("M-<up>" . flymake-goto-prev-error))
;;   :config
;;   (setq flymake-wrap-around nil))

(use-package markdown-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "C-c C-l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  (lsp-enable-which-key-integration t)
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t))

(use-package flycheck
  :bind
  (("M-<down>" . flycheck-next-error)
   ("M-<up>" . flycheck-previous-error)))

(define-key global-map (kbd "M-\"") #'xref-find-definitions)
(define-key global-map (kbd "M-«") #'xref-find-references)
(define-key global-map (kbd "M-$") #'xref-go-back)

;; (use-package lsp-tailwindcss
;;   :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss")
;;   :init (setq lsp-tailwindcss-add-on-mode t))

;;;;;;;;
;; Go ;;
;;;;;;;;

(defun go-before-save-hook ()
  (when (eq major-mode 'go-mode)
	(gofmt-before-save)
	(lsp-organize-imports)))

(defun setup-before-save-hooks ()
  (add-hook 'before-save-hook #'go-before-save-hook))

(use-package go-mode
  :hook
  (go-mode . yas-minor-mode)
  (go-mode . phindent-mode)
  (go-mode . whitespace-mode)
  (go-mode . lsp-deferred)
  (go-mode . setup-before-save-hooks))

;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;

(add-hook 'html-mode-hook 'infer-indentation-style)
(add-hook 'html-mode-hook (lambda () (setq tab-width 2)))

;;;;;;;;;
;; CSS ;;
;;;;;;;;;

(add-hook 'css-mode-hook 'infer-indentation-style)
(add-hook 'css-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'css-mode-hook 'phindent-mode)

;;;;;;;;;
;; Lua ;;
;;;;;;;;;

(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;;;;;;;;
;; Coq ;;
;;;;;;;;;

(ignore-errors (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))

;;;;;;;;;;
;; Agda ;;
;;;;;;;;;;

(ignore-errors (load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate"))))
(ignore-errors '(agda2-program-args (quote ("+RTS" "-K40M" "-RTS"))))

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
(ignore-errors (require 'haskell-mode-autoloads))
(ignore-errors (require 'haskell-mode))
(add-to-list 'Info-default-directory-list "/usr/share/emacs/site-lisp/haskell-mode/")

;;;;;;;;;;;;;;;;;;;;;
;; C (for NetHack) ;;
;;;;;;;;;;;;;;;;;;;;;

(defun hook-c ()
  (setq c-set-style "k&r")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'knr-argdecl-intro 0))
(add-hook 'c-mode-common-hook 'hook-c)

;; Correct path
(add-to-list 'exec-path "/usr/local/bin/")

;; Maximize
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

; Local configuration
(when (file-exists-p "~/.emacs.d/init-local.el")
  (load-file "~/.emacs.d/init-local.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
 '(LaTeX-math-abbrev-prefix "*")
 '(TeX-close-quote "\"")
 '(TeX-command-BibTeX "Biber")
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "evince %s.pdf" TeX-run-discard t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Biber" "biber %s.bcf" TeX-run-Biber nil t)))
 '(TeX-electric-sub-and-superscript nil)
 '(TeX-fold-macro-spec-list
   '(("[f]"
      ("footnote"))
     ("[i]"
      ("index"))
     ("*"
      ("item"))
     ("..."
      ("dots"))
     (1
      ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))
 '(TeX-fold-type-list '(env macro comment))
 '(TeX-newline-function 'reindent-then-newline-and-indent)
 '(TeX-open-quote "\"")
 '(TeX-output-view-style
   '(("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "evince %o")
     ("^html?$" "." "netscape %o")))
 '(agda2-highlight-level 'interactive)
 '(agda2-include-dirs '("." ".."))
 '(agda2-program-args '("+RTS" "-M25G" "-RTS"))
 '(asm-comment-char 59)
 '(compilation-ask-about-save nil)
 '(compilation-read-command t)
 '(coq-prog-name "/usr/bin/coqtop")
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   '("78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" default))
 '(fill-column 80)
 '(haskell-mode-hook '(turn-on-haskell-indent))
 '(home-end-enable t)
 '(js-indent-align-list-continuation nil)
 '(js2-strict-missing-semi-warning t)
 '(lsp-tailwindcss-major-modes
   '(rjsx-mode web-mode html-mode css-mode typescript-mode ultimate-js-mode))
 '(ns-alternate-modifier 'none)
 '(ns-command-modifier 'meta)
 '(outline-regexp "[
]*" t)
 '(preview-auto-cache-preamble t)
 '(preview-default-preamble
   '("\\RequirePackage["
     ("," . preview-default-option-list)
     "]{preview}[2004/11/05]"))
 '(preview-preserve-counters t)
 '(preview-scale-function 1.3)
 '(read-quoted-char-radix 16)
 '(safe-local-variable-values
   '((comment-fill-column . 80)
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (make-local-variable 'coq-prog-name)
           (setq coq-prog-name
                 (expand-file-name "../hoqtop")))))
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(tab-width 4)
 '(tree-sitter-debug-highlight-jump-region t)
 '(tree-sitter-debug-jump-buttons t)
 '(vc-annotate-color-map
   '((20 . "#FA99B7")
     (40 . "#FE9A9F")
     (60 . "#F89F88")
     (80 . "#ECA777")
     (100 . "#D9AF6D")
     (120 . "#C2B86D")
     (140 . "#A8BF75")
     (160 . "#8BC485")
     (180 . "#6CC89C")
     (200 . "#49C9B5")
     (220 . "#21C9CE")
     (240 . "#0EC7E4")
     (260 . "#3CC3F5")
     (280 . "#6ABDFE")
     (300 . "#95B5FE")
     (320 . "#BAACF6")))
 '(warning-suppress-log-types '((comp) (lsp-mode) (lsp-mode)))
 '(warning-suppress-types '((lsp-mode) (lsp-mode)))
 '(whitespace-style '(face trailing lines-tail empty tabs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:underline (:color "ForestGreen" :style wave)))) t))

(load-file "~/.emacs.d/mytheme.el")
(enable-theme 'mytheme)
