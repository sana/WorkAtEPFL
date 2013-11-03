#!/bin/bash

scala fos.FJ "
class A extends Object
{
    C x1;
    Object x2;

    A(C x1, Object x2)
    {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    Object foo(Object x1)
    {
        return new A(this.x1, x1);
    }
}

class B extends A
{
        B x3;
        A y;
        Object z;

    B(C x1, Object x2, B x3, A y, Object z)
    {
        super(x1, x2);
        this.x3 = x3;
                this.y=y;
                this.z =z;
    }

    Object foo(Object puta)
    {
        return this.y;
    }
}

class C extends Object
{
    C()
    {
        super();
    }
    
    B bar(B x1, C x2, C x3)
    {
        return x1;
    }

    C foo()
    {
        return new C();
    }
}

(A) (Object) (A) new A(new C() , new A(new C().foo(), new Object()).foo(new Object()) ).foo(new C())
"
