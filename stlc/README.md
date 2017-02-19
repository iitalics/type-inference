
# Simply Typed Lambda Calculus

## Usage

Run example program:

    $ racket example.rkt

#lang REPL:

    $ racket -int stlc.rkt


## Syntax

Datum:

    > 4
    4 : integer
    > "hello"
    hello : string

Lambda:

    > (lambda (x : A) x)
    #<procedure> : (A -> A)
    > (lambda (x : A) (lambda (y : B) x))
    #<procedure> : (A -> (B -> A))

Application:

    > +
    #<procedure:+> : (integer -> (integer -> integer))
    > (+ 3)
    #<procedure> : (integer -> integer)
    > ((+ 3) 4)
    7 : integer

Let binding:

    > (let (x : integer = 4) x)
    4 : integer
    > (let (f : (integer -> integer)
              = (lambda (x : integer)
                  (* x x)))
        (f 4))
    16 : integer
