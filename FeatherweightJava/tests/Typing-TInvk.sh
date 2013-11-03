#!/bin/bash

cd classes

scala fos.FJ "
class D extends Object
{
    Object x1;

    D(Object x1)
    {
        super();
        this.x1 = x1;
    }

    Object foo(Object x1)
    {
        return x1;
    }
}

class E extends Object
{
    E()
    {
        super();
    }
}

new D(new Object()).foo(new E())
"

