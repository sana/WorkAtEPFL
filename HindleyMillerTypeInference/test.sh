#!/bin/bash

cd classes

scala fos.Infer "0"
scala fos.Infer "true"
scala fos.Infer "false"
scala fos.Infer "succ 0"
scala fos.Infer "pred 0"
scala fos.Infer "iszero 0"
scala fos.Infer "iszero 1"

scala fos.Infer "if true then 0 else 1"
scala fos.Infer "if iszero 0 then 1 else 0"
scala fos.Infer "if iszero pred 0 then true else false"

scala fos.Infer "if iszero succ 0 then true else 0"
scala fos.Infer "if 0 then 0 else 1"
scala fos.Infer "if iszero ((\\x . if true then x else 1) 1) then false else true"

scala fos.Infer "x"
scala fos.Infer "\\x . if iszero x then 0 else 1"
scala fos.Infer "\\x . if x then 0 else 1"

scala fos.Infer "\\x. \\y . x y"
scala fos.Infer "\\x. \\y . y x"

scala fos.Infer "\\x. \\y . x y"

scala fos.Infer "\\x . (\\x . x) x"
scala fos.Infer "\\z . \\y . z (y true)"
scala fos.Infer "\\w . if true then false else w false"
scala fos.Infer "\\f . \\a . f (f a)"
scala fos.Infer "\\x . \\y . \\z . (x z) (y z)"

scala fos.Infer "\\x . \\y . x"
scala fos.Infer "\\x . \\y . y"

scala fos.Infer "\\x . \\y . if iszero 0 then (\\x . x y) (\\x . x) else 0"
scala fos.Infer "\\x . (\\x . x) x"

scala fos.Infer "let double = \\f . \\x . f(f(x)) in
    if (double (\\x . if x then false else true) false)
        then double (\\x . succ x) 0
        else 0"

scala fos.Infer "let double = \\x . x in double 0"
scala fos.Infer "let id = \\y . y in let y = id in y id"
scala fos.Infer "let x = 0 in let y = iszero x in if y then y else y"
scala fos.Infer "\\x . let indent = \\x . let indent = \\x . if x then 1 else 2 in indent x in indent (iszero x)"

# Not typable
scala fos.Infer "\\x. x x"
scala fos.Infer "(\\x . x x)(\\y . y)"

scala fos.Infer "\\x . \\y . (x y) (y x)"
scala fos.Infer "\\x . \\y . \\z . x (y z)"
scala fos.Infer "\\x . \\y . \\z . (x y) z"
scala fos.Infer "\\x . \\y . x (y x)"

scala fos.Infer "\\x . x (\\y . y x)"
scala fos.Infer "\\x . \\y . \\z . if (x y) then y else z"
scala fos.Infer "\\x . \\y . \\z . x (if (y z) then (z x) else true)"
scala fos.Infer "\\x . \\y . (\\z . x y)"
scala fos.Infer "\\x . x (\\y . \\z . y)"
scala fos.Infer "\\f . ((\\x . f (x x)) (\\x . f (x x)))"
scala fos.Infer "\\x . \\y . x (\\z . y x)"

