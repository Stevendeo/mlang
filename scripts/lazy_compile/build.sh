mkdir -p _build
cp -f src/*.ml* _build/
opam exec -- ocamlfind ocamlc \
     -package str -package unix -package cmdliner \
     -linkpkg \
     -I _build \
     -o _build/lcc \
     _build/cli.mli _build/cli.ml \
     _build/utils.mli _build/utils.ml \
     _build/dep_graph.mli _build/dep_graph.ml \
     _build/main.ml &&
cp _build/lcc lcc
