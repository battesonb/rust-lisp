# Rust Lisp

An attempt at writing a Lisp lexer, parser and interpreter as a simple learning
experience! However, the interpreter hides a lot of complexity relating to making the
language compile.

This follows some of the syntax and semantics from Common Lisp, but not all
of it. See the tests directory for expected deviations. The attempt here is to
build a language with as few "special" operations and surprises as possible.
`setq`, `lambda`, and a few others specifically don't evaluate some of their
arguments, which makes them surprising. One option here is to switch to
requiring quoted symbols as input, but I've dropped that for now.

## Grammars

There is no special syntax besides brackets and words/symbols. A simple grammar
for the lexer:

```bnf
r_paren  := ')'
l_parent := '('
symbol   := (<letter> | <digit>) - (<r_paren> | <l_paren>)
```

Whitespace is ignored, and anything after `;` is treated as a comment.

The parser has a slightly more complicated grammar, but also quite simple:

```bnf
exprs := (<expr>\s)*
expr  := <r_paren> expr <l_parent> | (<symbol>\s)*
```

## Language features

There is no native concept of a function. All functions are just a `lambda`.
`defun` is just a macro (via `defmacro`) for setting a variable to a given
lambda. There is only one scope for functions and variables.
