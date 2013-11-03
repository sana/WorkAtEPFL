#!/bin/bash

cd classes

scala fos.FJ "
class A extends Object
{
    Object x1;

    A(Object x1)
    {
        super();
        this.x1 = x1;
    }

    Object foo(A x1)
    {
        return x1;
    }
}

class B extends Object
{
    A()
    {
        super();
    }
}

(new A(new B())).x1
"

