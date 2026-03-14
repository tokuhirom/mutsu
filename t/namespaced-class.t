use Test;

plan 4;

class A::B {
    has $.x;
}

is A::B.new(x => 1).x, 1, 'fully qualified class declaration supports .new';

package Outer {
    class Inner {
        has $.value;
    }
}

is Outer::Inner.new(value => 7).value, 7, 'nested package class supports .new via namespace';

my $type = Outer::Inner;
is $type.^name, 'Outer::Inner', 'type object keeps fully qualified name';
is $type.new(value => 9).value, 9, 'type object from namespaced class can instantiate';
