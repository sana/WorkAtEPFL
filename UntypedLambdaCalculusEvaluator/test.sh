#!/bin/bash

cd classes

#scala fos.Untyped "x"

#scala fos.Untyped "\\x . y"

#scala fos.Untyped "(\\y . (z x))"
#scala fos.Untyped "\\x . y z"

#scala fos.Untyped "\\x . ( \\y . ((x y) x))"
#scala fos.Untyped "x"
#scala fos.Untyped "x y"

#scala fos.Untyped "\\x . \\y . z y"
#scala fos.Untyped "(\\x . x) ((\\x . x) (\\z. (\\x . x) z))"

# True and False
#scala fos.Untyped "\\t . \\f . t"
#scala fos.Untyped "\\t . \\f . f"

## Test a value
#scala fos.Untyped "\\l . \\m . \\n . l m n"

## test tru v w 
scala fos.Untyped "(\\l . \\m . \\n . l m n) (\\t . \\f . t) v w"

# tru v w
scala fos.Untyped "(\\t . \\f . t) v w"

# fls v w
scala fos.Untyped "(\\t . \\f . f) v w"


## AND
#scala fos.Untyped "\\b. \\c. b c (\\t . \\f . f)"
## 
scala fos.Untyped "(\\b. \\c. b c (\\t . \\f . f)) (\\t.\\f.t) (\\t.\\f.f)"


## Pair
#scala fos.Untyped "\\f . \\s . \\b . b f s"

# First
#scala fos.Untyped "\\p . p (\\t . \\f . t)"
# Second
#scala fos.Untyped "\\p . p (\\t . \\f . f)"
# fst (pair v w) : 
scala fos.Untyped "(\\p . p (\\t . \\f . t)) ((\\f . \\s . \\b . b f s) v w)" 



## Numerals
#scala fos.Untyped "\\s . \\z . z"

#scala fos.Untyped "\\s . \\z . s z"
#scala fos.Untyped "\\s . \\z . s s z"

# Successor
#scala fos.Untyped "\\n . \\s . \\z . s (n s z)"

# Test successor
#scala fos.Untyped "((\\s. (\\z.(s z))) s) z"


scala fos.Untyped "(\\n . \\s . \\z . s (n s z)) (\\s . \\z . s z)"

##plus:
scala fos.Untyped "(\\m. \\n. \\s. \\z. m s (n s z)) (\\s . \\z . s (s z)) (\\s . \\z . s (s (s z)))"


#times:
#scala fos.Untyped "\\x. \\y. x ((\\m. \\n. \\s. \\z. m s (n s z)) y) (\\s . \\z . z)"

# 3x3 : HARD TEST
scala fos.Untyped "(\\m. \\n. m ((\\m. \\n. \\s. \\z. m s (n s z)) n) (\\s . \\z . z)) (\\s . \\z . s (s (s z))) (\\s . \\z . s (s (s z)))"

#iszero:
#scala fos.Untyped "(\\m. m (\\x. (\\t . \\f . f)) (\\t.\\f . t))"

scala fos.Untyped "(\\m. m (\\x. (\\t . \\f . f)) (\\t.\\f . t)) (\\s.\\z.s (s z))"



### LAST TEST :D 

#zz = pair c0 c0
#ss = λp. pair (snd p) (scc (snd p))
#prd = λm. fst (m ss zz)

#prd 3
scala fos.Untyped \
"(\\m. (\\p . p (\\t . \\f . t)) \
        (m \
            (\
                \\p.(\\f . \\s . \\b . b f s) \
                        ((\\p . p (\\t . \\f . f))  p )  \
                        (  (\\n . \\s . \\z . s (n s z)) ((\\p . p (\\t . \\f . f))  p ) )\
            )\
                \
            (\
                (\\f . \\s . \\b . b f s)  (\\s . \\z . z) (\\s . \\z . z)\
\
            )\
        )\
 )\
 (\\s . \\z . s (s (s z)))\
"

