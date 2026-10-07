use Test;

# From the SeqSplitter distribution: a multi method signature naming a role
# declared inside the same class resolves it from the declaring package, so a
# role that composes `Iterator` is narrower than `Iterator`.

plan 3;

class Outer {
    role SI does Iterator {
        multi method from(SI $i) { "SI" }
        multi method from(Iterator $i) { "Iterator" }
    }
    class C does SI { method pull-one { IterationEnd } }
    class P does Iterator { method pull-one { IterationEnd } }
    method narrow { C.from(C.new) }
    method wide { C.from(P.new) }
    method inst { C.new.from(C.new) }
}

is Outer.narrow, 'SI', 'type-object invocant, narrower role wins';
is Outer.wide, 'Iterator', 'a plain Iterator takes the wide candidate';
is Outer.inst, 'SI', 'instance invocant';
