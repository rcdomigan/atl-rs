atl
===

One day, I hope this will be A Typed Lisp; the ATL language.  For now
it's a barely functioning and feature incomplete VM and byte-code
(well, enum) compiler which is being ported to Rust (because it's a
hobby project and why not).

build and run
=============

`cargo run` to get a simple REPL.

Language
========
Such as it is, integers and a few basic functions are supported.
- `sub2`: Subtract the second argument from the first
- `add2`: Add two arguments
- `equal`: Compare two integers for equality
- `\`: Declare an anonymous function.  Tuple de-structuring is supported (eg `(\ ((Tuple a b c)) (add2 a (add2 b c)))`).
- `define`: define an expression (define a function with `(define func (lambda (...) (...)))`)
- `Tuple`: define a tuple (eg `(Tuple 1 2 3)`).
- `exit`: quit the VM
- `if`: an if statement (eg `(if True 1 2)`)
- `True`: boolean true (name will probably change to #t)
- `False`: boolean false (name will probably change to #f)
- `:`: declare a type (eg `((: (-> Num Num Num) add2) 1 2)`).  The
  annotations are parsed into the Ast but currently ignored after
  that.

