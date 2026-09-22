use Test;

plan 2;

class Outer {
    my @pairs = ('left', 'right');

    class Inner {
        method values { @pairs }
    }
}

is-deeply Outer::Inner.new.values.List, <left right>,
    'nested class methods read an enclosing class-body array lexical';
is Outer::Inner.new.values.elems, 2,
    'the enclosing array lexical remains a container value';
