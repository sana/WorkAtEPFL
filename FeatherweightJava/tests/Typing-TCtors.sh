#!/bin/bash

cd classes

# This gives stackoverflow, even if it should report cycle in classes' hierarchy
scala fos.FJ "
class C extends E
{
    C()
    {
        super();
    }
}

class E extends C
{
    E()
    {
        super();
    }
}

new C()
"

