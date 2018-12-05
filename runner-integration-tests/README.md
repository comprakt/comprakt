|--------------|-------------------------------------------------|----------------------|
| Folder       | Description                                     | Cli Equivalent       |
|--------------|-------------------------------------------------|----------------------|
| lexer        | Assert stdin/stdout/exitcode of the lexer       | `--lextest`          |
|--------------|-------------------------------------------------|----------------------|
| parser       | Assert stdin/stdout/exitcode of parser. Note    | `--parsetest`        |
|              | that errors may change since the parser pulls   |                      |
|              | the lexer.                                      |                      |
|--------------|-------------------------------------------------|----------------------|
| semantic     | Assert stderr/exitcode of a semanatic analysis. | `--check`            |
|              | stdout should always be empty.                  |                      |
|--------------|-------------------------------------------------|----------------------|
| ast          | Same as semantic analysis, but prints           |                      |
|              | normalized MiniJava on stdout. Normalized       |                      |
|              | MiniJava is feed into the compiler and checked  |                      |
|              | for idempotency.                                |                      |
|--------------|-------------------------------------------------|----------------------|
| assembly     | Assert stderr/stdout/exitcode and the generated | `--lower --emit-asm` |
|              | assembly file.                                  |                      |
|--------------|-------------------------------------------------|----------------------|
| binary       | Assert stderr/stdout/exitcode for binary        |                      |
|              | generation (a full compiler call). Run the      |                      |
|              | generated binary and assert stderr/stdout       |                      |
|              | and exitcode.                                   |                      |
|--------------|-------------------------------------------------|----------------------|
| timeout      | Assert stderr/stdout/exitcode for binary        |                      |
|              | generation (a full compiler call). Run the      |                      |
|              | generated binary and assert that it does never  |                      |
|              | return.                                         |                      |
|--------------|-------------------------------------------------|----------------------|
| optimization | Compile the binary once with optimizations,     |                      |
|              | once without optimizations. Assert that the     |                      |
|              | emitted assembly changed/did not change/matches |                      |
|              | the assembly of another program. Assert that    |                      |
|              | the behaviour of the binary is identical to the |                      |
|              | unoptimized binary.                             |                      |
|--------------|-------------------------------------------------|----------------------|
