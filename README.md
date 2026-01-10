# Rust Lisp

[View the web demo!](https://battesonb.github.io/rust-lisp/)

An attempt at writing a Lisp lexer, parser and interpreter as a simple learning
experience! However, the interpreter hides a lot of complexity relating to
making the language compile.

This follows some of the syntax and semantics from Common Lisp, but not all
of it. See the tests directory for expected deviations. The attempt here is to
build a language with as few "special" operations and surprises as possible.
`setq`, `lambda`, and a few others specifically don't evaluate some of their
arguments, which makes them surprising. One option here is to switch to
requiring quoted symbols as input, but I've dropped that for now.

## Grammars

There is no special syntax besides brackets and words/symbols. A simple (not
perfectly accurate) grammar for the lexer:

```bnf
r_paren  := ')'
l_parent := '('
symbol   := (<letter> | <digit>) - (<r_paren> | <l_paren>)
string   := ".*"
```

Whitespace is ignored, and anything after `;` is treated as a comment.

The parser has a slightly more complicated grammar, but also quite simple:

```bnf
exprs := (<expr>\s)*
expr  := <r_paren> exprs <l_parent> | <symbol> | <string>
```

## Language features

There is no native concept of a function. All functions are just a `lambda`.
`defun` is just a macro (via `defmacro`) for setting a variable to a given
lambda.

There is only one scope for functions and variables, making this a Lisp-1
implementation (instead of a Lisp-2, like Common Lisp). This actually has
surprising issues, so you have to be careful with variable names clashing with
method names like `list`. Not great!

See `std.lisp` for extensions/bootstrapping of the language. Some of the native
functions don't actually work until `std.lisp` bootstraps the interpreter, so it
is a required module before anything else is loaded/called.

The types of values are surprisingly broad: Strings, numbers, symbols, cons
cells, functions, native functions and macros.
