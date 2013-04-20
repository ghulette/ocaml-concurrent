SOURCES = mvar.mli mvar.ml \
          ivar.mli ivar.ml
RESULT  = concurrent
THREADS = yes

all: native-code-library byte-code-library

include OCamlMakefile
