FROM ocaml/opam

RUN sudo apt install -y pkg-config libgmp-dev libgmp-ocaml-dev
RUN opam install dune base cohttp cohttp-lwt-unix js_of_ocaml js_of_ocaml-ppx lwt ppx_blob stdio
# RUN eval $(opam env)
ENV OPAM_SWITCH_PREFIX '/home/opam/.opam/4.13'
ENV CAML_LD_LIBRARY_PATH '/home/opam/.opam/4.13/lib/stublibs:/home/opam/.opam/4.13/lib/ocaml/stublibs:/home/opam/.opam/4.13/lib/ocaml'
ENV OCAML_TOPLEVEL_PATH '/home/opam/.opam/4.13/lib/toplevel'
ENV PATH '/home/opam/.opam/4.13/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'

# Cloning RustScript
RUN git clone https://github.com/mkhan45/RustScript2
WORKDIR RustScript2

# Build/install RustScript
RUN dune build
RUN mkdir -p ~/.local/bin
RUN cp _build/default/bin/rustscript_cli.exe ~/.local/bin/rustscript
ENV PATH $PATH:~/.local/bin
