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
}

class C extends D
{
    B(Object x1)
    {
        super(x1);
    }
}

(D)(new C(new Object()))
"

