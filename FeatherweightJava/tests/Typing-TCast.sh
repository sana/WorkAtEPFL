#!/bin/bash

cd classes

scala fos.FJ "
class B extends Object
{
    B()
    {
        super();
    }
}

class C extends Object
{
    C()
    {
        super();
    }
}

class E extends C
{
    E()
    {
        super();
    }

    Object foo()
    {
        return new B();
    }
}

class F extends E
{
    F()
    {
        super();
    }

    Object foo()
    {
        return new C();
    }
}

((F) (Object) new E()).foo()
"

# T-UCast
#(E) new F()

# T-DCast
#((F) (Object) new E()).foo()

# T-SCast
#(B) new C()
