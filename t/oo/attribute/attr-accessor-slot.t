use v6;
use Test;

# Attribute accessors are slot-based (BLOCKERS §3.2 item 2):
# - `my $ref := $obj.attr` binds the attribute's container (bidirectional)
# - `$obj.attr.VAR does Role` mixes into the attribute container; the mixin
#   attribute is readable and rw-writable through `.VAR.name`
# All expectations verified against rakudo.

plan 18;

class A {
    has $.x is rw = 10;
    method bump { $!x++ }
    method get  { $!x }
}

# --- bind to rw scalar accessor: container identity ---
{
    my $obj = A.new;
    my $ref := $obj.x;
    is $ref, 10, 'bound ref reads the attribute value';
    $obj.x = 20;
    is $ref, 20, 'accessor write is visible through the bound ref';
    $ref = 30;
    is $obj.x, 30, 'ref write is visible through the accessor';
    $obj.bump;
    is $ref, 31, 'method-internal $!x++ is visible through the bound ref';
    $ref = 50;
    is $obj.get, 50, 'ref write is visible to a method-internal $!x read';
    is $obj.gist, 'A.new(x => 50)', 'gist renders the inner value, not the cell';
    is $obj.raku, 'A.new(x => 50)', 'raku renders the inner value, not the cell';

    # clone detaches from the shared slot
    my $c = $obj.clone;
    $c.x = 99;
    is $obj.x, 50, 'clone write does not reach the original attribute';
    is $ref, 50, 'clone write does not reach the bound ref';
}

# --- .VAR distinguishes rw container from non-rw value ---
{
    class B { has $.y = 5; has $.z is rw = 6 }
    my $b = B.new;
    is $b.y.VAR.^name, 'Int', 'non-rw accessor result has no container (.VAR is the value)';
    is $b.z.VAR.^name, 'Scalar', 'rw accessor result is a Scalar container';

    my $r := $b.y;
    throws-like { $r = 9 }, Exception, 'assigning through a value-bound ref dies';
    is $b.y, 5, 'the non-rw attribute is untouched';
}

# --- typed rw attribute: constraint travels with the bound container ---
{
    class C { has Int $.x is rw = 1 }
    my $c = C.new;
    my $rc := $c.x;
    throws-like { $rc = "foo" }, Exception, 'type check enforced through the bound ref';
    is $c.x, 1, 'failed assignment leaves the attribute untouched';
    $rc = 42;
    is $c.x, 42, 'conforming assignment through the bound ref succeeds';
}

# --- mixin on the attribute container via .VAR does ---
{
    role Named { has $.name is rw = "anon" }
    my $obj = A.new;
    $obj.x.VAR does Named;
    is $obj.x.VAR.name, 'anon', 'role attribute readable through .VAR after does';
    $obj.x.VAR.name = "bob";
    is $obj.x.VAR.name, 'bob', 'rw write through .VAR.name updates the mixin';
}
