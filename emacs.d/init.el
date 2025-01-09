;;;;;;;;;;;;;;;;;
;; Straight.el ;;
;;;;;;;;;;;;;;;;;

;; Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure the use-package macro to work with straight
(use-package straight
  :custom (straight-use-package-by-default t))

;;;;;;;;;;;;;;;;;;;;;
;; Global settings ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :init
  (global-whitespace-mode 1)
  (setq whitespace-global-modes '(not mediawiki-mode))
  (setq inhibit-startup-message t) ;; No welcome message
  (setq initial-major-mode 'markdown-mode)
  (setq initial-scratch-message "\
# Temporary notes
")
  (tool-bar-mode -1) ;; No tool bar
  (menu-bar-mode -1) ;; No menu bar
  (setq visible-bell t) ;; Visible bell

  ;; Customized window title
  (setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

  (column-number-mode 1) ;; Show column number

  ;; Auto-fill by default in text-mode
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; Delete selection when typing
  (pending-delete-mode)

  ;; Consider dashes to be a word constituent in the minibuffer
  (modify-syntax-entry ?\- "w" minibuffer-local-filename-syntax)

  ;; Confirm before killing Emacs
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; No backup files
  (setq make-backup-files nil)

  ;; No lockfiles
  (setq create-lockfiles nil)

  ;; ;; Auto save files in ~/.emacs.d/auto-save
  ;; (defvar autosave-dir (concat user-emacs-directory "/auto-save/"))
  ;; (make-directory autosave-dir t)
  ;; (setq auto-save-file-name-transforms
  ;;       `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

  ;; Automatically show images and compressed files
  (setq auto-image-file-mode t)
  (setq auto-compression-mode t)

  ;; Don’t require typing "yes"/"no" entirely
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Scrolling
  (mouse-wheel-mode 1)
  (pixel-scroll-precision-mode 1)

  ;; History for minibuffer
  (savehist-mode)

  ;; ;; Deal with long lines (disabled for now as Emacs 29 supposedly fixes it)
  ;; (global-so-long-mode 1)

  ;; Mark script files as executable automatically
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Tooltips in the echo area
  (tooltip-mode -1)

  ;; Correct path
  (add-to-list 'exec-path "/usr/local/bin/")

  ;; Maximize
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; Local configuration
  (when (file-exists-p "~/.emacs.d/init-local.el")
    (load-file "~/.emacs.d/init-local.el"))

  ;; Select help buffers automatically
  (setq help-window-select t)

  ;; Electricity
  (electric-pair-mode 1)
  (electric-indent-mode 1))

(use-package paren
  :init
  (setq show-paren-delay 0)
  (setopt show-paren-predicate t)
  (setopt show-paren-context-when-offscreen 'overlay)
  (setopt show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

(defun duplicate-lines ()
  "Duplicate the lines intersecting the region."
  (interactive)
  (save-excursion
    (let* ((start
            (save-excursion
              (goto-char (region-beginning))
              (beginning-of-line)
              (point)))
           (end
            (save-excursion
              (goto-char (region-end))
              (end-of-line)
              (point)))
           (line (buffer-substring start end)))
      (goto-char (region-end))
      (forward-line 1)
      (beginning-of-line)
      (insert line "\n"))))
(global-set-key (kbd "C-c C-j") #'duplicate-lines)

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

(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (LaTeX-math-mode 1)
   (TeX-PDF-mode 1)
   (TeX-fold-mode 1)
   (setq TeX-parse-self t)
   (setq TeX-auto-save t)
   (set-fill-column 100)))
(ignore-errors (load "auctex.el" nil t t))
(ignore-errors (load "preview-latex.el" nil t t))

(add-hook
 'LaTeX-mode-hook
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

;;;;;;;;;;;;;;;;;;
;; Shell script ;;
;;;;;;;;;;;;;;;;;;

(use-package shell-script-mode
  :straight nil
  :mode "\\(\\.sh\\'\\|zshrc\\)"
  :init
  (setq sh--completion-keywords '("if" "then" "elif" "else" "fi" "while" "until" "for" "in" "do" "done" "case" "esac")))

;;;;;;;;;
;; Git ;;
;;;;;;;;;

;; (use-package magit)

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

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package phindent-mode
  :straight (:host github :repo "guillaumebrunerie/phindent-mode"))

(defun infer-indentation-style (defaults-to-tabs)
  "Detects and sets indentation style (tabs vs spaces), and width (four for
tabs, and supports two and four for spaces)

We consider the file to be indented with spaces if there are strictly more lines
starting with two spaces than lines starting with a tab.

In the case of spaces, we consider it to be indented with two spaces if there is
at least one line starting with exactly two spaces.

Special cases to support:
- An empty file (or a file without indented lines) is indented with tabs.
- A file with one predominant indentation style but small mistakes here and
there should still be identified correctly.
"
  (let* ((tabs-indented-lines (how-many "^\t" (point-min) (point-max)))
         (spaces-indented-lines (how-many "^  " (point-min) (point-max)))
         (two-spaces-indented-lines (how-many "^  [^ \t]" (point-min) (point-max)))
         (no-indented-lines (and (= tabs-indented-lines 0) (= spaces-indented-lines 0))))
    (setq-local indent-tabs-mode (if no-indented-lines defaults-to-tabs (>= tabs-indented-lines spaces-indented-lines)))
    (setq-local js-indent-level (cond
                                 (indent-tabs-mode 4)
                                 ((and no-indented-lines (not defaults-to-tabs)) 2)
                                 ((= two-spaces-indented-lines 0) 4)
                                 (t 2)))
    (setq-local sgml-basic-offset js-indent-level)
    (setq-local typescript-ts-mode-indent-offset js-indent-level)))

(defun infer-indentation-style-defaulting-to-tabs ()
  (infer-indentation-style t))

(defun infer-indentation-style-defaulting-to-spaces ()
  (infer-indentation-style nil))

(require 'typescript-ts-mode)
(require 'js)

(use-package ultimate-js-mode
  :straight (:host github :repo "guillaumebrunerie/ultimate-js-mode" :files (:defaults "libs" "queries"))
  :mode ("\\.\\([mc]?[jt]sx?\\|json\\)\\'" . ultimate-js-mode)
  :interpreter ("bun" . ultimate-js-mode)
  :hook
  (ultimate-js-mode . infer-indentation-style-defaulting-to-tabs)
  (ultimate-js-mode . phindent-mode)
  (ultimate-js-mode . copilot-mode)
  (ultimate-js-mode . lsp-deferred)
  ;; (ultimate-js-mode . eglot-ensure)
  ;; (ultimate-js-mode . (lambda () (flymake-eslint-enable)))
  :config
  ;; (defun js--continued-expression-p () nil)
  (setq js--declaration-keyword-re "\\<\\(let\\|var\\)\\>")
  (setq js-switch-indent-offset js-indent-level))

(use-package apheleia
  :config
  (setq apheleia-mode-lighter " Aph")
  (setf (alist-get 'ultimate-js-mode apheleia-mode-alist)
        '(prettier))
  (apheleia-global-mode +1))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :config
  (setq copilot-indent-offset-warning-disable t)
  :ensure t)
;; (("C-<tab>" . my/hs-toggle)
;;  ("<backtab>" . my/hs-close)
;;  ("C-S-<tab>" . my/hs-open)
;;  ("<C-iso-lefttab>" . my/hs-open))

(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-line)
(define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)

(add-to-list 'copilot-indentation-alist '(ultimate-js-mode js-indent-level))

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

;; (use-package corfu
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-prefix 1)
;;   (corfu-auto-delay 0.1)
;;   (corfu-quit-no-match t)
;;   (corfu-on-exact-match 'quit)
;;   :bind (("s-<tab>" . #'completion-at-point)
;;          ("A-<tab>" . #'completion-at-point)
;;          :map corfu-map
;;          ([remap move-beginning-of-line] . nil)
;;          ([remap move-end-of-line] . nil)
;;          ([remap beginning-of-buffer] . nil)
;;          ([remap end-of-buffer] . nil)
;;          ([remap scroll-down-command] . nil)
;;          ([remap scroll-up-command] . nil)
;;          ([remap next-line] . nil)
;;          ([remap previous-line] . nil)
;;          ([remap keyboard-escape-quit] . corfu-quit)
;;          ("C-a" . nil)
;;          ("M-n" . nil)
;;          ("M-p" . nil)
;;          ("<tab>" . nil))
;;   :init
;;   (global-corfu-mode))

;; (use-package corfu-prescient)

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind (("s-<tab>" . #'completion-at-point)
         ("A-<tab>" . #'completion-at-point))
  :custom
  (company-minimum-prefix-length 1)
  ;; No auto complete after numbers
  (company-idle-delay (lambda () (if (looking-back "\\b[0-9]+" (- (point) 5)) nil 0.1))))

(use-package company-prescient
  :config
  (company-prescient-mode +1))

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

(defun my/hs-set-up-overlay (ov)
  (overlay-put
   ov
   'display
   (propertize
    (format
     "...%d lines hidden..."
     (count-lines (overlay-start ov) (overlay-end ov)))
    'face
    'hs-folded)))

(defface hs-folded '((t :background "#782200" :foreground "#EEE" :weight bold :box "#FF0000")) "Dots")

;; (use-package hs-minor-mode
;;   :straight nil
;;   :bind
;;   (("C-<tab>" . my/hs-toggle)
;;    ("<backtab>" . my/hs-close)
;;    ("C-S-<tab>" . my/hs-open)
;;    ("<C-iso-lefttab>" . my/hs-open))
;;   :custom
;;   (hs-hide-comments-when-hiding-all nil)
;;   :init
;;   (setq hs-set-up-overlay 'my/hs-set-up-overlay)
;;   :config
;;   (hs-minor-mode))

(use-package prog-mode
  :straight nil
  :hook (prog-mode . hs-minor-mode))

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(use-package markdown-mode)


;;;;;;;;;;;;;;;;;;;;;
;; Language Server ;;
;;;;;;;;;;;;;;;;;;;;;

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(ultimate-js-mode . ("typescript-language-server" "--stdio"))))

;; (defun my-next-error ()
;;   "Show next diagnostic"
;;   (interactive)
;;   (next-line)
;;   (call-interactively 'flymake-show-diagnostic))

;; (defun my-prev-error ()
;;   "Show previous diagnostic"
;;   (interactive)
;;   (previous-line)
;;   (call-interactively 'flymake-show-diagnostic))

;; (defun my-show-diagnostics ()
;;   "Show project diagnostics"
;;   (interactive)
;;   (flymake-show-project-diagnostics)
;;   (other-window 1)
;;   (call-interactively 'flymake-show-diagnostic))

;; (use-package flymake
;;   :bind
;;   (("M-<down>" . flymake-goto-next-error)
;;    ("M-<up>" . flymake-goto-prev-error)
;;    ("C-c C-l" . my-show-diagnostics)
;;    :map flymake-diagnostics-buffer-mode-map
;;    :map flymake-project-diagnostics-mode-map
;;    ("n" . my-next-error)
;;    ("p" . my-prev-error))
;;   :config
;;   (setq flymake-wrap-around nil))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :custom
  ;; (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "C-c C-l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)
  (lsp-dired-mode)
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-diagnostic-max-lines 10))

(use-package flycheck
  :bind
  (("M-<down>" . flycheck-next-error)
   ("M-<up>" . flycheck-previous-error)))

(define-key global-map (kbd "M-\"") #'xref-find-definitions)
(define-key global-map (kbd "M-«") #'xref-find-references)
(define-key global-map (kbd "M-$") #'xref-go-back)

;;;;;;;;;;;;;;;;;
;; Tree sitter ;;
;;;;;;;;;;;;;;;;;

(setq
 treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun treesit-install-my-language-grammars ()
  (interactive)
  (treesit-install-language-grammar 'javascript)
  (treesit-install-language-grammar 'typescript)
  (treesit-install-language-grammar 'tsx)
  (treesit-install-language-grammar 'json)
  (treesit-install-language-grammar 'css)
  (treesit-install-language-grammar 'html)
  (treesit-install-language-grammar 'go)
  (treesit-install-language-grammar 'elisp))

(defvar-local expand-region--nodes nil)
(defvar-local expand-region--previous-point nil)

(defun expand-region ()
  (interactive)
  (when (not (region-active-p))
    (setq expand-region--previous-point (point))
    (setq expand-region--nodes nil))
  (let ((node (if (consp expand-region--nodes)
                  (treesit-node-parent (car expand-region--nodes))
                (treesit-node-at (point)))))
    (when node
      (setq expand-region--nodes (cons node expand-region--nodes))
      (set-mark (treesit-node-start node))
      (goto-char (treesit-node-end node))
      (setq transient-mark-mode (cons 'only transient-mark-mode)))))

(defun unexpand-region ()
  (interactive)
  (setq expand-region--nodes (cdr expand-region--nodes))
  (when (region-active-p)
    (if (consp expand-region--nodes)
        (let ((node (car expand-region--nodes)))
          (set-mark (treesit-node-start node))
          (goto-char (treesit-node-end node))
          (activate-mark))
      (progn
        (deactivate-mark)
        (goto-char expand-region--previous-point)))))

(define-key global-map (kbd "M-h") #'expand-region)
(define-key global-map (kbd "M-H") #'unexpand-region)

(use-package multiple-cursors
  :bind
  (("C-<next>" . mc/mark-next-like-this)))

;; (use-package combobulate)
;; (load "combobulate")
;; (setq combobulate-js-ts-enable-auto-close-tag nil)
;; (combobulate-mode)

;; Colors
(use-package rainbow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real time SVG editing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst realtime-svg--image-buffer "*svg-image*")

(defun realtime-svg--update-image ()
  "Turn the current buffer's content into an image and display it in a new buffer."
  (let ((svg (buffer-string)))
    (with-silent-modifications
      (with-current-buffer (get-buffer-create realtime-svg--image-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert svg)
          (image-mode)
          (set-buffer-modified-p nil)
          (display-buffer (current-buffer) '((display-buffer-in-direction) (direction . right))))))))

(defvar realtime-svg--timer nil)
(defvar realtime-svg--debounce-delay 0.1)
(defun realtime-svg--change-handler (&rest _args)
  "Handler for `after-change-functions` to update SVG image."
  (when realtime-svg--timer
    (cancel-timer realtime-svg--timer))
  (setq realtime-svg--timer
        (run-with-idle-timer realtime-svg--debounce-delay nil #'realtime-svg--update-image)))

(defun realtime-svg--setup ()
  "Set up real time SVG editing."
  (when (string= image-type "svg")
    (realtime-svg--change-handler)
    (add-hook 'after-change-functions #'realtime-svg--change-handler nil t)))

;; Enable real time SVG editing
(add-hook 'image-minor-mode-hook #'realtime-svg--setup)


;;;;;;;;;;;;;;;;;;;
;; Atomic Chrome ;;
;;;;;;;;;;;;;;;;;;;

(use-package visual-fill-column)
(use-package mediawiki
  :bind
  (:map mediawiki-mode-map
        ("C-<left>" . nil)
        ("C-<right>" . nil)
        ("C-<up>" . nil)
        ("C-<down>" . nil)
        ("C-x C-s" . 'ignore)
        ("M-q" . 'ignore)
        ("M-g" . nil))
  :config
  ;; workaround for bug https://github.com/hexmode/mediawiki-el/issues/36
  (remove-hook 'outline-minor-mode-hook 'mediawiki-outline-magic-keys))

(use-package atomic-chrome
  :init
  (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-default-major-mode 'mediawiki-mode)
  :hook
  (atomic-chrome-edit-mode . visual-fill-column-mode)
  (atomic-chrome-edit-mode . visual-line-mode)
  (atomic-chrome-edit-mode . turn-off-auto-fill)
  (atomic-chrome-edit-mode . (lambda () (setq fill-column 100))))

;;;;;;;;
;; Go ;;
;;;;;;;;

(defun go-before-save-hook ()
  (when (eq major-mode 'go-mode)
    (gofmt-before-save)
    (lsp-organize-imports)))
;; (eglot-code-action-organize-imports)))

(defun my/add-package-line-if-missing ()
  (when (and (string-match-p "\\.go\\'" (buffer-name))
             (eq (point-min) (point-max))) ;; Check if buffer is empty
    (let ((package-name (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))))
      (insert (format "package %s\n\n" package-name)))))

(defun setup-before-save-hooks ()
  (add-hook 'before-save-hook #'go-before-save-hook))

(use-package go-mode
  :hook
  (go-mode . phindent-mode)
  (go-mode . lsp-deferred)
  ;; (go-mode . eglot-ensure)
  (go-mode . setup-before-save-hooks)
  (go-mode . my/add-package-line-if-missing)
  (go-mode . (lambda () (treesit-parser-create 'go))))

;;;;;;;;;
;; XML ;;
;;;;;;;;;

(defun xml-format ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "xmllint --format -" t t))

;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;

(add-hook 'html-mode-hook 'infer-indentation-style-defaulting-to-spaces)
(add-hook 'html-mode-hook 'phindent-mode)
(add-hook 'html-mode-hook #'turn-off-auto-fill)

;;;;;;;;;
;; CSS ;;
;;;;;;;;;

(add-hook 'css-mode-hook 'infer-indentation-style-defaulting-to-spaces)
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

;;;;;;;;;;;
;; Elisp ;;
;;;;;;;;;;;

(defun elisp-hook ()
  (setq indent-tabs-mode nil)
  (treesit-parser-create 'elisp))
(add-hook 'emacs-lisp-mode-hook #'elisp-hook)

;;;;;;;;;;;;;;;;;;;;;
;; C (for NetHack) ;;
;;;;;;;;;;;;;;;;;;;;;

(defun hook-c ()
  (setq c-set-style "k&r")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'knr-argdecl-intro 0))
(add-hook 'c-mode-common-hook 'hook-c)

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
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'none)
 '(outline-regexp "[\12]*" t)
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
