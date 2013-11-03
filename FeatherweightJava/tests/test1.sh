#!/bin/bash

cd classes

scala fos.FJ "
class A extends Object
{
    C x1;
    Object x2;

    A(B x1, C x2)
    {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    C foo()
    {
        return x1;
    }
}

class B extends A
{
    B(C x1, Object x2, B x3)
    {
        super(x1, x2);
        this.x2 = x2;
    }

    Object foo()
    {
        return x2;
    }
}

class C extends Object
{
    C()
    {
        super();
    }
    
    B bar(Object x1, C x2, C x3)
    {
        return x1;
    }

    Object foo()
    {
        return x1;
    }
}

(new C()).foo()
"
#((A) new C()).foo()
#((C) new Object()).bar((new A(new C(), new Object())).foo(), new C(), new C())

#(new C()).bar(new A(new C(), new Object()), new C())
#(new A(new C(), new Object())).foo()
#new A(new A(new C(), new C()), new Object())
#new A(new C(), new A(new C(), new C() ))
#(new A(new Object(), new Object())).x1
#(C) new A()
#((A) new B(new Object())).foo()


