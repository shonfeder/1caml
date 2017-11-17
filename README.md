# 1Caml

1Caml is an implementation of [1ML](https://people.mpi-sws.org/~rossberg/1ml/) targeting the OCaml runtime

## Status

This implementation is a work in progress and doesn't do much yet.

## Installing

### Prerequisites

| prerequisite | | version | how to install |
|-|-|:-|-|
| Opam | `>=` | [`2.0.0~beta4`](https://github.com/ocaml/opam/releases/tag/2.0.0-beta4) | manually or via package manager |
| OCaml | `==` | `4.05.0+flambda` | `opam switch create 4.05.0+flambda` |
| JBuilder | `>=` | [`1.0+beta16`](https://github.com/janestreet/jbuilder/releases/tag/1.0%2Bbeta16) | `opam install jbuilder` |

### Building

```
$ jbuilder external-lib-deps --missing @install    # install deps
$ jbuilder build @install                          # build artifacts
$ jbuilder exec 1caml                              # invoke CLI
```

## Contributing

Please see the following documents.

- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing](CONTRIBUTING.md)