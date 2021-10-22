opam init -y
eval $(opam env --switch=default)
opam install dune -y
opam install base stdio -y # Add deps here
eval $(opam config env)