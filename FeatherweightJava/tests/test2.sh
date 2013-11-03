#!/bin/bash

scala fos.FJ "
class A extends Object { A() { super(); } }
class B extends Object { B() { super(); } }
class Pair extends Object {
  Object fst;
  Object snd;
  Pair(Object fst, Object snd) {
    super(); this.fst=fst; this.snd=snd; }
  Pair setfst(Object newfst) {
    return new Pair(newfst, this.snd); }
}

new Pair(new A(), (A) new B()).setfst(new Object()).fst

"
