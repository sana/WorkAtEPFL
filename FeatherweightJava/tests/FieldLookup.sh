#!/bin/bash

cd classes

scala fos.FJ "
class B extends Object
{
    Object x1;

    B(Object x1)
    {
        super();
        this.x1 = x1;
    }

    Object foo()
    {
        return this.x1;
    }
}

class C extends B
{
    Object x2;

    C(Object x1, Object x2)
    {
        super(x1);
        this.x2 = x2;
    }

    Object foo()
    {
        return this.x2;
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

new C(new D(), new E()).x1
"

