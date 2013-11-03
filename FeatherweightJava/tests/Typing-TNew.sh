#!/bin/bash

cd classes

scala fos.FJ "
class D extends Object
{
    Object x1;
    Object x2;
    Object x3;

    D(Object x1, Object x2, Object x3)
    {
        super();
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }
}

class E extends Object
{
    E()
    {
        super();
    }
}

class F extends E
{
    F()
    {
        super();
    }
}

new D(new Object(), new E(), new F())
"

