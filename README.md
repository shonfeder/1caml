# 1Caml

1Caml is an implementation of [1ML](https://people.mpi-sws.org/~rossberg/1ml/)
targeting the OCaml runtime

## Status

This implementation is a work in progress and doesn't do much yet.

## Installing

### Prerequisites

| prerequisite |      | version                                                                | how to install                  |
| ------------ | ---- | :--------------------------------------------------------------------- | ------------------------------- |
| Opam         | `>=` | [`1.2.2`](https://github.com/ocaml/opam/releases/tag/1.2.2)            | manually or via package manager |
| OCaml        | `>=` | [`4.06.1+flambda`](https://github.com/ocaml/ocaml/releases/tag/4.06.1) | `opam switch 4.06.1+flambda`    |
| utop         | `>=` | [`2.0.2`](https://github.com/diml/utop/releases/tag/2.0.2)             | `opam install utop` (optional)  |

### Installing Dependencies

```
$ git clone https://github.com/1caml/1caml
$ cd 1caml
$ opam update
$ opam pin add -y .
```

### Building

```
$ make
```

### Toplevel

Requires `utop` (see prerequisites).

```
$ make top
```

### Tests

```
$ make test
```

## Contributing

Please see the following documents.

* [Code of Conduct](CODE_OF_CONDUCT.md)
* [Contributing](CONTRIBUTING.md)
