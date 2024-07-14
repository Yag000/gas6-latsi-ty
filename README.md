# [GAS6] LATSI Project 2023-2024

## Authors

This project was developed by Yago Iglesias Vazquez and Tony Ly Soan as part of the "Grammars and Syntax Analysis" course during the sixth semester of the Bachelor's in Computer Science at Université Paris Cité.

## Dependencies

The main dependencies are `opam`, `dune`, `ocaml`, and `menhir`.

To set up the necessary environment, you can create a local `opam` switch with the following command:

```shell
opam switch create . --deps-only
```

Before proceeding, ensure your desired `opam` environment is active in your shell:

```shell
eval $(opam env)
```

**Additional dependencies are required for testing the project:**

- If you have already created the local `opam` switch, install the new dependencies with:

```shell
opam install . --deps-only --with-test
```

- Otherwise, create a local `opam` switch at the project root with the necessary dependencies:

```shell
opam switch create . --deps-only --with-test
```

## How to Run the Project

### Compilation

To compile the program, run the following command from the project root:

```shell
dune build
```

#### Note

Sometimes, before compiling and/or testing, you might need to remove the `_build` directory with:

```shell
dune clean
```

### Execution

To execute our implementations from the project root, you can use either:

- Our standard implementation:

```shell
dune exec latsi <your LATSI file>
```

- Our implementation with extensions:

```shell
dune exec latsi_extension <your LATSI file>
```

#### Note

Ensure the content of `<your LATSI file>` ends with an empty newline to adhere to LATSI syntax.

### Testing

We have rigorously tested our project with unit tests and property tests using the libraries `alcotest`, `qcheck`, and `ppx_expect`.

To run the tests for our implementations from the project root, use:

- Unit tests for our standard implementation:

```shell
dune runtest test_latsi
```

- Unit tests for our implementation with extensions:

```shell
dune runtest test_extension
```

- All unit tests:

```shell
dune runtest
```

## Directory and File Organization

```shell
-lib
 ├── latsi                    <== Standard implementation
 └── latsi_extension          <== Implementation with extensions

-test_latsi                   <== Tests for standard version
 └── expect-tests             <== Output tests for LATSI programs

─test_extension               <== Tests for extended version
 └── expect-tests             <== Output tests for LATSI programs
```

