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

    D bar()
    {
        return this;
    }
}

class C extends D
{
    B(Object x1)
    {
        super(x1);
    }
}

class A extends Object
{
    A()
    {
        super();
    }

    D foo()
    {
        return new D(this);
    }
}

(new A()).foo().bar()
"

