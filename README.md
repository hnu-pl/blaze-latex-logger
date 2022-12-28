# blaze-latex-logger
Simple code example to demonstrate logging with LaTeX output (and somthing more),
with minimal features (just wrtie on one file, no automatic log rotating, etc.)
and Haskell package dependencies (must use external log display server).

![screen-record](https://github.com/kyagrd/blaze-latex-logging-example/blob/main/screen-record.gif?raw=true)

Features:
- renders simple LaTeX formulae (using either KaTeX or MathJax, depending on your choice) 
- basic tree rendering (using Treeflex)
- auto scrolls to the last content (i.e., bottom of the html document)

See `app/Main.hs` and `src/Text/Blaze/Html/Logger.hs` for details.


For logging only those simple formulae rederable by client side LaTeX renderer (KaTeX/MathJax), vscode's internal markdown viewer assisted by AutoScroll vscode plugin is good enough. However, adding additional lightwegit features rederable using clident side JS (e.g., tree/graph rendering) requires full dive into developing vscode plugin enhancing vscode's markdown preview, which may not be cost effective for small scale projects or experimental prototyping stage.
