# GA6-LATSI-TY

## How to run the code

The code depends on `menhir`, `ocamllex` and `dune`, in particular it uses the version `2.1`. You can install yourself the dependencies
on your global switch or create a local switch and install the dependencies:

```bash
opam switch create . --deps-only
```

Then you can build the project with the following command:

```bash
dune build bin/main.ml
```

and run the code with the following command:

```bash
./_build/default/bin/main.exe <filename>
```

Where `<filename>` is the name of the file you wish to execute.

## Tests

The code has been thoroughly tested using `alcotest`, `qcheck` and `ppx_expect`. If you wish to run the tests, you will need to install them.
The easiest way to do this is to create a local switch and install the dependencies:

```bash
opam switch create . --deps-only --with-test  
```

Then you can run the tests with the following command:

```bash
dune runtest
```
