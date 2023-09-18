# Tries, staged
Provides efficient\* generic implementation of a trie data structure, using [staged generics](https://github.com/modular-implicits/staged-generics)
for low-overhead generic code generation.

<sup>\*efficiency is aspirational, not actual</sup>

The method for implementing the tries is taken from the paper [Memo Functions, polytypically!](https://citeseerx.ist.psu.edu/doc/10.1.1.43.3272)
by Ralf Hinze, in Workshop on Generic Programming, 2000. It is the same technique used in the
[MemoTrie](https://github.com/conal/MemoTrie) library for Haskell.

## Build instructions
This library requires OCaml with both the [modular implicits](https://github.com/ocamllabs/ocaml-modular-implicits)
and [BER MetaOCaml](https://okmij.org/ftp/meta-programming/ber-design.pdf) extensions.

You can install that variant of OCaml using `opam`:

```bash
opam switch add modular-implicits-ber 4.02.1+modular-implicits-ber
```

Getting this to build is a bit tricky.
The `HACK` directory contains a shim to convince Dune to use `metaocamlc` instead of `ocamlc`.
You need to add it to your path. For example:

```bash
PATH="$(pwd)/HACK:$PATH" dune build
```

There is probably a better way of doing this.
It appears to be unnecessary in modern versions of Dune, but those don't support this old version of OCaml.
