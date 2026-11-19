use Test;
use MONKEY-SEE-NO-EVAL;

# A method installed with `.^add_method` from a closure literal sees its
# defining scope. The call installs only the names its body can look up
# (free variables, names its ops carry, its own locals); a body that creates
# closures or names lexicals reflectively still gets the whole scope.

plan 10;

class C { has $.v = 3 }

sub install(::T, $offset, $label) {
    my $unused = 'never read';
    my $counter = 0;
    my sub helper($x) { "$label:$x" }
    C.^add_method('plain', my method () { self.v + $offset });
    C.^add_method('callee', my method () { helper(self.v) });
    C.^add_method('typed', my method () { T.^name });
    C.^add_method('nested', my method () { (1, 2).map({ $_ + $offset }).List });
    C.^add_method('counts', my method () { ++$counter });
    C.^add_method('reflective', my method () { EVAL '$label' });
    C.^add_method('outer-ref', my method () { $OUTER::label });
    C.^add_method('defaulted', my method ($x = $offset) { $x });
    C.^compose;
}

install(Int, 10, 'L');
my $c = C.new;

is $c.plain, 13, 'a captured scalar is visible';
is $c.callee, 'L:3', 'a captured lexical sub is callable by bare name';
is $c.typed, 'Int', 'a captured type capture resolves';
is-deeply $c.nested, (11, 12), 'a closure inside the method sees the captured scope';
$c.counts;
is $c.counts, 2, 'a write to a captured variable persists across calls';
is $c.reflective, 'L', 'EVAL inside the method sees the captured scope';
is $c.outer-ref, 'L', 'OUTER:: inside the method reaches the captured scope';

is $c.defaulted, 10, 'a parameter default reads the captured scope';

# Repeated calls keep answering from the captured scope.
is (^3).map({ $c.plain }).join(','), '13,13,13', 'repeated calls see the capture';

# A caller lexical of the same name does not leak into the method.
my $offset = 1000;
is $c.plain, 13, "the caller's same-named lexical is not the method's";
