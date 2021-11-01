(require 'package)
(package-initialize)

; No welcome message
(setq inhibit-startup-message t)

; Cleaner interface
(tool-bar-mode -1)
(menu-bar-mode -1)

; Mettre un titre aux fenêtres
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

; Afficher le numéro de colonne
(column-number-mode 1)
(linum-mode 1)

; Mode texte en auto-fill par défaut
; (créé une nouvelle ligne à chaque fois que vous taper du texte)
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

;(if (and (boundp 'window-system) window-system)
;    (require 'font-lock))


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
  '(lambda () (modify-syntax-entry ?_ "w" caml-mode-syntax-table)))

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (define-key tuareg-mode-map "\M-q" 'fill-paragraph)
))

(add-hook 'tuareg-load-hook
          '(lambda ()
             (define-key tuareg-mode-map [f9] 'tuareg-eval-buffer)
             (define-key tuareg-mode-map [f10] 'tuareg-eval-phrase)
             (outline-minor-mode)
             (define-key tuareg-mode-map [M-left] `hide-subtree)
             (define-key tuareg-mode-map [M-right] `show-subtree)
             ))

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


;;;;;;;;;;;;
;; YamlPP ;;
;;;;;;;;;;;;

(fset 'êf-yamlpp-fr
   [?< ?# ?f ?r ?> ?< ?/ ?# ?f ?r ?> left left left left left left])
(fset 'êo-yamlpp-eo
   [?< ?# ?e ?o ?> ?< ?/ ?# ?e ?o ?> left left left left left left])
(fset 'êe-yamlpp-en
   [?< ?# ?e ?n ?> ?< ?/ ?# ?e ?n ?> left left left left left left])

;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

(global-whitespace-mode 1)

(load-file "~/.emacs.d/js2-highlight-vars.el")
(add-hook 'js2-mode-hook #'js2-highlight-vars-mode)
(add-hook 'js2-mode-hook (lambda () (setq js-switch-indent-offset 4)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (setq indent-tabs-mode (if (> space-count tab-count) nil t))))

(add-hook 'js2-mode-hook 'infer-indentation-style)

(add-hook 'rjsx-mode (lambda () (seqt indent-tabs-mode t)))

;; But not tabs by default, as it messes up various other modes (e.g.
;; elisp and XML)
(setq-default indent-tabs-mode nil)

(load-file "~/.emacs.d/phindent-mode/phindent-mode.el")

(add-hook 'js2-mode-hook 'phindent-mode)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

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

;;;;;;;;;;;;
;; PovRay ;;
;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/pov-mode-3.2")
(autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

;;;;;;;;;;;;
;; Maxima ;;
;;;;;;;;;;;;

(push "/usr/local/share/emacs/site-lisp" load-path)
(push "/usr/share/maxima/5.13.0/emacs" load-path)
(autoload 'imaxima  "imaxima" "Image support for Maxima." t)
(autoload 'maxima "maxima" "Frontend for maxima" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

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

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (setq ispell-tex-skip-alists
                   (list
                     (append
                       (car ispell-tex-skip-alists) 
                       '(("[^\\]\\$" . "[^\\]\\$") ("\\[" . "\\]") ("\\\\operatorname" ispell-tex-arg-end)))
                     (append
                       (cadr ispell-tex-skip-alists)
                       '(("align\\*?" . "\\\\end{align\\*?}")
                         ))))))

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

(load-file "~/.emacs.d/mytheme.el")
(enable-theme 'mytheme)

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
 '(fill-column 100)
 '(haskell-mode-hook '(turn-on-haskell-indent))
 '(home-end-enable t)
 '(js2-strict-missing-semi-warning t)
 '(ns-alternate-modifier 'none)
 '(ns-command-modifier 'meta)
 '(outline-regexp "[ 
]*" t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(web-mode lsp-mode rjsx-mode markdown-mode go-mode rainbow-mode autothemer js2-refactor js2-mode nhexl-mode visual-fill-column csharp-mode yaml-mode ess haskell-mode auctex))
 '(preview-auto-cache-preamble t)
 '(preview-default-preamble
   '("\\RequirePackage["
     ("," . preview-default-option-list)
     "]{preview}[2004/11/05]"))
 '(preview-preserve-counters t)
 '(preview-scale-function 1.3)
 '(read-quoted-char-radix 16)
 '(safe-local-variable-values
   '((eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (make-local-variable 'coq-prog-name)
           (setq coq-prog-name
                 (expand-file-name "../hoqtop")))))
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(tab-width 4)
 '(whitespace-style '(face trailing lines-tail empty tabs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
