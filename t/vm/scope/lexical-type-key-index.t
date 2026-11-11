use Test;

# Lexical (`my`) types are stored under mangled `Name\0<id>` keys; the lookups
# that map a source spelling to those keys answer from an index of the keys
# rather than walking every registered type (#17331). The answers must be what
# a scan would give: a unique `my` type resolves, a nested one too, and two
# same-named `my` types in different scopes stay apart.

plan 7;

my class Lone { method who { 'lone' } }
is Lone.new.who, 'lone', 'a my class resolves';
ok Lone.new ~~ Lone, 'and type-checks';

my class Outer {
    my class Inner { method who { 'inner' } }
    method make { Inner.new }
}
is Outer.make.who, 'inner', 'a my class nested in a my class';

{
    my class Twin { method who { 'first' } }
    is Twin.new.who, 'first', 'a block-scoped my class';
}
{
    my class Twin { method who { 'second' } }
    is Twin.new.who, 'second', 'a same-named one in another block stays apart';
}

sub typed(Lone $l) { $l.who }
is typed(Lone.new), 'lone', 'a my class as a parameter type';
my Lone @list = Lone.new, Lone.new;
is @list.elems, 2, 'and as an array element type';
