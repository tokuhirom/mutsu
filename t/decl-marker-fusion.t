use v6;
use Test;

# Pin for the declaration-marker peephole (ADR-0006 §2.3): `my $x = <expr>`
# compiles to one fused `SetLocalDecl` instead of
# `MarkExplicitInitializerContext; MarkVarDeclContext; SetLocal`. The fused
# instruction must set exactly the same two context flags, so every behaviour
# that depends on "this store is a declaration, and it had an initializer" has
# to be unchanged.

plan 12;

# --- a declaration is not a reassignment ----------------------------------
# A `my` inside a loop re-declares a fresh variable each iteration; it must not
# be treated as a mutation of the outer one.
my $outer = 'outer';
for 1..2 {
    my $outer = 'inner';
    is $outer, 'inner', 'the inner declaration shadows the outer one';
}
is $outer, 'outer', 'the outer variable is untouched by the shadowing declaration';

# --- explicit initializer vs bare declaration -----------------------------
my $bare;
ok !$bare.defined, 'a bare declaration leaves the variable undefined';

my Int $typed = 3;
is $typed, 3, 'a typed declaration with an initializer stores its value';

my Int $untyped_bare;
ok !$untyped_bare.defined, 'a typed bare declaration is undefined, not 0';

# Redeclaring without an initializer keeps the value; with one, it re-runs.
my $keep = 1;
{
    my $keep = 2;
    is $keep, 2, 'an initialized redeclaration in an inner block takes the new value';
}
is $keep, 1, 'the outer value survives';

# --- declarations captured by a closure -----------------------------------
# The free-variable analysis distinguishes declaration from mutation by the
# marker the fused instruction now carries.
my $counter = 0;
my &bump = -> { $counter++ };
bump(); bump();
is $counter, 2, 'a closure still mutates the captured outer variable';

sub make-counter() {
    my $n = 0;             # declaration inside the routine
    return -> { ++$n };    # captured and mutated
}
my &c = make-counter();
c();
is c(), 2, 'a captured routine-local declaration keeps its own cell';

# --- a declaration whose initializer is a container ------------------------
my @list = 1, 2, 3;
is @list.elems, 3, 'an array declaration with an initializer';
my %h = a => 1;
is %h<a>, 1, 'a hash declaration with an initializer';

done-testing;
