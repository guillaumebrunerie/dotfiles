(straight-use-package 'autothemer)
(require 'autothemer)

(autothemer-deftheme
 mytheme "My theme"

 ((((class color) (min-colors #xFFFFFF)))

  (mytx        "#efefef")     ; Good
  (myoperator  "#bbb")
  (mybg        "#1e1e1e")     ; Good
  (mykeyword   "DarkViolet")  ; Good
  (myconstant  "DarkCyan")
  (mybuiltin   "DarkCyan")
  (mycomment   "#b11")        ; Good
  (mydoc       "#f00")
  (mystring    "#e70")        ; Good
  (myvar       "#da6")        ; Good
  (mypropvar   "#db8")
  (myfunname   "#04f")        ; Good
  (mytype      "#0A0")
  (mytypeparam "#F60")
  (myparam     "#092")        ; Good
  (mytab       "#282828")     ; Good
  (myline      "#383838")     ; Good
  (myregion    "#1d3854")     ; Good
  (myhvars     "#070")        ; Good
  (mynamespace "#04F")
  (mysearch    "Blue")
  (myjsdctype  "Red")
  (myjsdcval   "Red")
  (mywarning   "#b50")
  (mywstrail   "#800"))

 ((default                      (:foreground mytx :background mybg :height 170))
  (font-lock-comment-face       (:foreground mycomment))
  (font-lock-constant-face      (:foreground myconstant))
  (font-lock-builtin-face       (:foreground mybuiltin))
  (font-lock-function-name-face (:foreground myfunname))
  (font-lock-keyword-face       (:foreground mykeyword :weight 'bold))
  (font-lock-string-face        (:foreground mystring))
  (font-lock-variable-name-face (:foreground myvar))
  (font-lock-doc-face           (:foreground mydoc))
  (font-lock-type-face          (:foreground mytype))
  (isearch                      (:background mysearch :distant-foreground mytx))
  (js2-external-variable        ())
  (js2-function-param           (:foreground myparam))
  (js2-highlight-vars-face      (:background myhvars :distant-foreground mytx))
  (js2-warning                  (:underline  mywarning))
  (region                       (:background myregion))
  (lazy-highlight               (:background myregion))
  (whitespace-line              (:background myline :foreground nil))
  (whitespace-tab               (:background mytab))
  (whitespace-trailing          (:background mywstrail))
  (tree-sitter-hl-face:operator (:foreground myoperator))
  (tree-sitter-hl-face:variable.parameter (:foreground myparam))
  (tree-sitter-hl-face:variable (:foreground myvar))
  (tree-sitter-hl-face:function (:foreground myvar))
  (tree-sitter-hl-face:property (:foreground mytx))
  (tree-sitter-hl-face:property.definition (:foreground mypropvar))
  (tree-sitter-hl-face:function.call (:foreground mytx))
  (tree-sitter-hl-face:constructor (:foreground mynamespace))
  (tree-sitter-hl-face:type (:foreground mytype))
  (tree-sitter-hl-face:type.parameter (:foreground mytypeparam))
  (tree-sitter-hl-face:punctuation.special (:foreground mystring :weight 'bold))
  (lsp-lsp-flycheck-info-unnecessary-face (:foreground nil :underline '(:style wave :color "ForestGreen")))

  (vterm-color-black   (:foreground "#2E3436" :background "#555753"))
  (vterm-color-red     (:foreground "#CC0000" :background "#EF2929"))
  (vterm-color-green   (:foreground "#4E9A06" :background "#8AE234"))
  (vterm-color-yellow  (:foreground "#C4A000" :background "#FCE94F"))
  (vterm-color-blue    (:foreground "#3465A4" :background "#729FCF"))
  (vterm-color-magenta (:foreground "#75507B" :background "#AD7FA8"))
  (vterm-color-cyan    (:foreground "#06989A" :background "#34E2E2"))
  (vterm-color-white   (:foreground "#FFFFFF" :background "#FFFFFF"))))

(defface vterm-background '((t :background "#300A24")) "Background for vterm")

(provide-theme 'mytheme)
