(straight-use-package 'autothemer)
(require 'autothemer)

(autothemer-deftheme
 mytheme "My theme"

 ((((class color) (min-colors #xFFFFFF)))

  (mytx        "#efefef")
  (myoperator  "#bbb")
  (mybg        "#1e1e1e")
  (mykeyword   "DarkViolet")
  (myconstant  "DarkCyan")
  (mybuiltin   "DarkCyan")
  (mycomment   "#b11")
  (mydoc       "#f00")
  (mystring    "#e70")
  (myvar       "#da6")
  (mypropvar   "#57B")
  (myfunname   "#05f")
  (mytype      "#0A0")
  (mytypeparam "#6D0")
  (mytab       "#282828")
  (myline      "#383838")
  (myregion    "#1d3854")
  (mynamespace "#05F")
  (mysearch    "Blue")
  (mywstrail   "#800")
  (myerror     "#BB0000")
  (mycssbg     "#030"))

 (
  ;; Syntax highlighting
  (default                       (:foreground mytx :background mybg :height 170))
  (font-lock-comment-face        (:foreground mycomment))
  (font-lock-constant-face       (:foreground myconstant))
  (font-lock-builtin-face        (:foreground mybuiltin))
  (font-lock-function-name-face  (:foreground myfunname))
  (font-lock-keyword-face        (:foreground mykeyword :weight 'bold))
  (font-lock-string-face         (:foreground mystring))
  (font-lock-variable-name-face  (:foreground myvar))
  (font-lock-doc-face            (:foreground mydoc))
  (font-lock-type-face           (:foreground mytype))
  (font-lock-operator-face       (:foreground myoperator))
  (font-lock-number-face         (:foreground myconstant))
  (font-lock-type-parameter-face (:foreground mytypeparam))
  (font-lock-tag-face            (:foreground mybuiltin))
  (font-lock-attribute-face      (:foreground mypropvar))
  (font-lock-warning-face        (:background myerror :foreground mytx))
  (ultimate-js-mode-embedded-lang-face (:background mycssbg :extend t))

  (tree-sitter-hl-face:operator (:foreground myoperator))
  (tree-sitter-hl-face:variable (:foreground myvar))
  (tree-sitter-hl-face:function (:foreground myvar))
  (tree-sitter-hl-face:property (:foreground mytx))
  (tree-sitter-hl-face:property.definition (:foreground mytx))
  (tree-sitter-hl-face:function.call (:foreground mytx))
  (tree-sitter-hl-face:constructor (:foreground mynamespace))
  (tree-sitter-hl-face:type (:foreground mytype))
  (tree-sitter-hl-face:type.parameter (:foreground mytypeparam))
  (tree-sitter-hl-face:punctuation.special (:foreground mystring :weight 'bold))
  (tree-sitter-hl-face:attribute (:foreground mypropvar))

  ;; Others
  (region                       (:background myregion))
  (lazy-highlight               (:background myregion))
  (whitespace-line              (:background myline :foreground 'unspecified))
  (whitespace-tab               (:background mytab))
  (whitespace-trailing          (:background mywstrail))
  (isearch                      (:background mysearch :distant-foreground mytx))

  ;; Diagnostics
  (lsp-lsp-flycheck-info-unnecessary-face (:foreground 'unspecified :underline '(:style wave :color "ForestGreen")))
  (lsp-flycheck-info-unnecessary-face (:foreground 'unspecified :underline '(:style wave :color "ForestGreen")))
  (lsp-flycheck-error-unnecessary-face (:foreground 'unspecified :underline '(:style wave :color "Red1")))
  (lsp-face-highlight-textual (:background "#34495E"))
  (lsp-face-highlight-read (:background "#34495E"))
  (lsp-face-highlight-write (:background "#34495E"))
  (flymake-warning (:underline '(:style wave :color "#E70")))
  (eglot-highlight-symbol-face (:background "#2C3E50"))
  (eglot-diagnostic-tag-unnecessary-face (:underline '(:style wave :color "#E70")))

  ;; Vterm
  (vterm-color-black   (:foreground "#2E3436" :background "#555753"))
  (vterm-color-red     (:foreground "#CC0000" :background "#EF2929"))
  (vterm-color-green   (:foreground "#4E9A06" :background "#8AE234"))
  (vterm-color-yellow  (:foreground "#C4A000" :background "#FCE94F"))
  (vterm-color-blue    (:foreground "#3465A4" :background "#729FCF"))
  (vterm-color-magenta (:foreground "#75507B" :background "#AD7FA8"))
  (vterm-color-cyan    (:foreground "#06989A" :background "#34E2E2"))
  (vterm-color-white   (:foreground "#FFFFFF" :background "#FFFFFF"))))

(defface vterm-background '((t :background "#300A24")) "Background for vterm")

(defface font-lock-type-parameter-face
  '((t . (:inherit font-lock-type-face)))
  "Face for type parameters.")

(defface font-lock-tag-face
  '((t . (:inherit font-lock-function-call-face)))
  "Face for HTML tags like <div> and <p> in JSX."
  :group 'typescript)

(defface font-lock-attribute-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for HTML attributes like name and id in JSX."
  :group 'typescript)

(provide-theme 'mytheme)
