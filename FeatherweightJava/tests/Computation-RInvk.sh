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

    Object foo(A x1, B x2)
    {
        return x2;
    }
}

class B extends Object
{
    A()
    {
        super();
    }
}

(new A(new B())).foo(new A(new Object()), new B())
"

