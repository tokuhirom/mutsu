use Test;

# A bareword naming a lexical (`my`) type or a bare enum key declared in an
# enclosing scope is remembered per chunk once resolved. Every site of a chunk
# that declares no type of its own sits on the same side of the declaring
# block, so the remembered answer is each site's answer. A chunk that does
# declare a same-named type keeps resolving per site. Every expectation here
# is rakudo's.

plan 8;

role R {
    my class Inner { method tag { 'inner' } }
    method make()  { Inner.new }
    method kind()  { Inner.^name }
}
class C does R {}

my $c = C.new;
is $c.make.tag, 'inner', 'a role-body my class read from a method';
is ($c.make.tag for ^3).join(','), 'inner,inner,inner', 'and on repeated calls';
is $c.kind, 'R::Inner', 'its name';
is $c.kind, 'R::Inner', 'its name, again';

class Inner { method tag { 'outer' } }

# One routine with a block-local `my class` of the same spelling: the sites
# inside and outside the block resolve differently, on every call.
sub both() {
    my @tags;
    { my class Inner { method tag { 'block' } }; @tags.push: Inner.tag }
    @tags.push: Inner.tag;
    @tags.join(',')
}
is both(), 'block,outer', 'a block-local my class shadows only inside its block';
is both(), 'block,outer', 'and still does on the next call';

# A role-body type read from a closure the method makes.
role R2 {
    my class Tok { method v { 42 } }
    method thunk() { -> { Tok.v } }
}
class C2 does R2 {}
my $t = C2.new.thunk;
is $t() + $t(), 84, 'a role-body my class read from a closure';
is C2.new.thunk()(), 42, 'from a fresh closure';
