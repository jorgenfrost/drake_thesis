(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "a4paper" "width=150mm" "top=25mm" "bottom=25mm") ("inputenc" "utf8") ("fontenc" "T1") ("caption" "font={small,it}")))
   (TeX-run-style-hooks
    "latex2e"
    "setup/temp_title"
    "setup/listofcontents"
    "introduction/introduction"
    "framework/framework"
    "methods/methods"
    "data/data"
    "appendix/rca"
    "appendix/fc_vs_hh_algorithm"
    "article"
    "art11"
    "geometry"
    "inputenc"
    "fontenc"
    "graphicx"
    "caption"
    "subcaption"
    "amsmath"
    "amssymb"
    "longtable"
    "natbib"
    "appendix")
   (LaTeX-add-bibliographies
    "setup/collection"))
 :latex)

