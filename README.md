# 1Caml

1Caml is an implementation of [1ML](https://people.mpi-sws.org/~rossberg/1ml/) targeting the OCaml runtime

## Status

This implementation is a work in progress and doesn't do much yet.

## Installing

### Prerequisites

| prerequisite | | version | how to install |
|-|-|:-|-|
| Opam | `>=` | [`2.0.0~beta4`](https://github.com/ocaml/opam/releases/tag/2.0.0-beta4) | manually or via package manager |

### Building

```
$ git clone https://github.com/1caml/1caml
$ cd 1caml
$ opam switch create . 4.05.0+flambda     # create a local ocaml switch
$ opam install . --deps-only              # install local dependencies
$ opam exec jbuilder -- build @install    # build artifacts
$ opam exec jbuilder -- exec 1caml        # invoke CLI
```

## Contributing

Please see the following documents.

- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing](CONTRIBUTING.md)
