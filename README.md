# Tries, staged
Provides efficient\* generic implementation of a trie data structure, using [staged generics](https://github.com/modular-implicits/staged-generics)
for low-overhead generic code generation.

<sup>\*efficiency is aspirational, not actual</sup>

The method for implementing the tries is taken from the paper [Memo Functions, polytypically!](https://citeseerx.ist.psu.edu/doc/10.1.1.43.3272)
by Ralf Hinze, in Workshop on Generic Programming, 2000. It is the same technique used in the
[MemoTrie](https://github.com/conal/MemoTrie) library for Haskell.
