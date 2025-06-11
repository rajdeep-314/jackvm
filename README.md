# Jack Virtual Machine

$$\Huge{\text{Jack VM Syntax}\to\text{Hack ASM}\to\text{Binary instructions}}$$

An OCaml program to translate JackVM programs to machine instructions for the Hack CPU, using Hack ASM as the intermediate language.

Also check out [Hack Assembler](https://github.com/rajdeep-314/hack-assembler) - the assembler used as a library for this project, to handle the translation from Hack ASM to machine instructions. This was written in OCaml as well.


# Features

## Multiple files

The translator can handle multiple files as input, easily resolving definitions across files. Read the [How to use?](#how-to-use?) section for how to invoke the translator with multiple files.


## Scalable

The translator is easily scalable. The use of polymorphic types plays a big role in this, because it means that types are parametrized, which provides flexibility and ease of implementation, without trading it with type-safety and rigor.

I plan to add RISCV as a target sometime in the future.


## Fast

OCaml's compiler makes this tranlsator extremely fast, while still providing high-level abstractions for better implementation.



# How to use?


Make sure that you have [Dune](https://dune.build/install), a build-system for OCaml, installed.

Once that is done, simply go to the project repository and execute the following to build the executable.

```bash
dune build jackvm
```

The executable can be invoked from the shell like so, where `my_file_1.vm` and `my_file_2.vm` contain JackVM code. The program will print the corresponding machine code to `stdout`, and output redirection can be used to redirect it to a file.

```bash
dune exec jackvm my_file_1.vm my_file_2.vm
```


### Note

- You may skip the building, as `dune exec` automatically builds the executable, in case it hasn't been built.
- Not passing any file names during the invocation prints the bootstrap code.


Some test cases can be found in the `test-snippets` directory.



