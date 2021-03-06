Programmatically or interactively inspect the AST nodes before lowering to
libFirm.

Invoke with `cargo run --bin inspect-ast` or `cargo run -p inspect-ast`.


```
$ cargo run -p inspect-ast -- --help
ast-inspector 0.1.0

USAGE:
    inspect-ast [FLAGS] [OPTIONS] <FILE>

FLAGS:
    -h, --help          Prints help information
    -1, --only-first    stop after first regex match
    -V, --version       Prints version information

OPTIONS:
    -c, --content <regex_content>        a regular expression used to filter the source code. Only matching AST elements
                                         will be dumped. Defaults to filtering nothing. [default: ]
    -k, --node-kind <regex_node_kind>    a regular expression used to filter the source code. Only matching AST elements
                                         will be dumped. Defaults to filtering nothing. [default: ]

ARGS:
    <FILE>    MiniJava file that should be parsed into an AST.
```

Allows you to filter based on AST-NodeKind and input source code. For example,
to get all type definitions and all statements containing 'abc':

```
$ cat ./mjtest-rs/tests/syntax/block_statement_vs_expression.mj
class Foo {
    /* Array definition vs access */
    public void bar() {
        abc[1];
        abc[] def;
    }

}
$ cargo run -p inspect-ast -- \
    ./mjtest-rs/tests/syntax/block_statement_vs_expression.mj \
    --content "abc" --node-kind "type|statement"

info: Matched kind 'statement'
   | 
 4 |         abc[1];
   |         ^^^^^^^

info: Matched kind 'statement'
   | 
 5 |         abc[] def;
   |         ^^^^^^^^^^

info: Matched kind 'type'
   | 
 5 |         abc[] def;
   |         ^^^^^

```
