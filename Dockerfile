FROM ocaml/opam:alpine AS build

RUN sudo apk add pkgconf libgmpxx gmp-dev binutils
RUN opam install dune base cohttp cohttp-lwt-unix js_of_ocaml js_of_ocaml-ppx lwt ppx_blob stdio safepass
# RUN eval $(opam env)
ENV OPAM_SWITCH_PREFIX '/home/opam/.opam/4.13'
ENV CAML_LD_LIBRARY_PATH '/home/opam/.opam/4.13/lib/stublibs:/home/opam/.opam/4.13/lib/ocaml/stublibs:/home/opam/.opam/4.13/lib/ocaml'
ENV OCAML_TOPLEVEL_PATH '/home/opam/.opam/4.13/lib/toplevel'
ENV PATH '/home/opam/.opam/4.13/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'

# Cloning RustScript
COPY . RustScript2
WORKDIR RustScript2
RUN sudo chown -R opam .

# Build RustScript
RUN dune build
RUN sudo strip _build/default/bin/rustscript_cli.exe

FROM alpine
COPY --from=build /home/opam/RustScript2/_build/default/bin/rustscript_cli.exe /bin/rustscript
