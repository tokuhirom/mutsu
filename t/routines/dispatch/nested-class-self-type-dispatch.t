use Test;

# Docker::File's nested instruction classes use their own short name in a
# method type constraint (`Label:D`).  This must dispatch on the nested class,
# not leave the constraint as the bare `Label` name.

plan 2;

class Outer {
    class Inner {
        multi method Str(Inner:D:) { 'inner' }
    }

    has Inner $.inner;
}

is Outer::Inner.new.Str, 'inner', 'nested class self type constraint dispatches';
is Outer.new(inner => Outer::Inner.new).inner.Str,
    'inner', 'nested class self type constraint also dispatches through an attribute';
