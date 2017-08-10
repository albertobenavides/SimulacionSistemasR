% title
% author(s) (separated by semicolons)
% date

# título 1

# título 2

## subtítulo

Nada *más*

* Una lista, si acaso.

pandoc -f markdown -o my.pdf test.md -V papersize=letter -V documentclass=article -V toc
