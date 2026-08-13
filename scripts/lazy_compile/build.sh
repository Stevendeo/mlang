opam exec -- ocamlfind ocamlc -package str -package unix -package cmdliner -linkpkg -o lcc utils.ml utils.mli dep_graph.mli dep_graph.ml cli.mli cli.ml main.ml
