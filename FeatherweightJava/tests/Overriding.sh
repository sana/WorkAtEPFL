#!/bin/bash

cd classes

scala fos.FJ "
class B extends Object
{
    B()
    {
        super();
    }

    Object foo(Object y, Object z)
    {
        return y;
    }
}

class C extends B
{
    C()
    {
        super();
    }

    Object foo(Object y, Object z)
    {
        return z;
    }
}

class D extends Object
{
    D()
    {
        super();
    }
}

class E extends Object
{
    E()
    {
        super();
    }
}

new C().foo(new D(), new E())
"

# The rule R-Cast is very weird !!! See the example
#((B) new C()).foo(new D(), new E())
