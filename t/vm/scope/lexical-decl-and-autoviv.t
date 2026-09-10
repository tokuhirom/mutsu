use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# Lexical declaration and autovivification corner cases:
#   * `my Type:D $x .= new` (a definedness smiley on a `.=` initializer)
#   * `my %h is default(V)` applied to a Nil pair value in a whole-hash assign
#   * chained (two-level) autovivification through an undefined scalar
#   * the process-wide `$*COLLATION` singleton

plan 46;

# --- `my Type:D $x .= new` -------------------------------------------------
# The smiley constrains the *variable*, not the invocant of the `.=` call, so
# the initializer must call `Type.new`, not the bareword `Type:D`.

{
    my Lock:D $lock .= new;
    isa-ok $lock, Lock, 'my Lock:D $x .= new builds a Lock';
    ok $lock.defined, 'my Lock:D $x .= new is defined';
}

{
    my Int:D $i .= new;
    is $i, 0, 'my Int:D $x .= new builds an Int';
}

{
    my Str:D $s .= new;
    is $s, "", 'my Str:D $x .= new builds a Str';
}

class DeclFoo { has $.x = 7 }

{
    my DeclFoo:D $f .= new;
    is $f.x, 7, 'my Foo:D $x .= new builds a user class instance';
}

{
    my DeclFoo $plain .= new;
    is $plain.x, 7, 'my Foo $x .= new (no smiley) still works';
}

# A `:U` smiley genuinely cannot hold the freshly constructed object.
dies-ok { EVAL 'my DeclFoo:U $u .= new' }, 'my Foo:U $x .= new fails the type check';

# --- `my %h is default(V)` and a Nil pair value ----------------------------
# Assigning a pair list into a defaulted hash stores each value INTO a fresh
# element container, so a Nil takes that container's default. A value that is
# already `Any` (including one a Hash RHS decayed) stays `Any`.

{
    my %h is default(42);
    %h = (a => 1, b => Nil);
    is %h<b>, 42, 'a Nil pair value takes the hash default';
    is %h<a>, 1, 'a non-Nil pair value is untouched';
    is %h<absent>, 42, 'an absent key still reads the default';
    ok %h<b>:exists, 'the Nil-valued key exists';
    nok %h<absent>:exists, 'the absent key does not exist';
    is %h.elems, 2, 'the defaulted store did not add a key';
}

{
    my %h is default(42);
    %h = a => 1, b => Nil;    # comma list, no parens
    is %h<b>, 42, 'a Nil pair value takes the default in a bare comma list too';
}

{
    my %h is default(42);
    %h<x> = Nil;              # direct element store
    is %h<x>, 42, 'a direct Nil element store takes the default';
}

{
    my %h is default(42);
    %h = (a => 1, b => Any);
    is %h<b>.raku, 'Any', 'an explicit Any pair value stays Any, not the default';
}

{
    my %h is default(42);
    %h = %(a => 1, b => Nil);  # a Hash RHS already decayed the Nil to Any
    is %h<b>.raku, 'Any', 'a Nil already decayed by a Hash RHS stays Any';
}

{
    my %h;
    %h = (a => 1, b => Nil);
    is %h<b>.raku, 'Any', 'with no default, a Nil pair value is Any';
}

{
    my @a is default(42);
    @a = (1, Nil, 3);
    is-deeply @a.List, (1, 42, 3).List, 'the array counterpart still applies its default';
}

# --- chained autovivification through an undefined scalar ------------------

{
    my $beatles;
    $beatles{"White Album"}[0] = "Back in the U.S.S.R.";
    is $beatles.raku, '${"White Album" => $["Back in the U.S.S.R."]}',
        'hash-then-array autovivification writes back into the root scalar';
    is $beatles{"White Album"}[0], "Back in the U.S.S.R.",
        'the autovivified value reads back';
}

{
    my $h;
    $h<a><b> = 1;
    is $h<a><b>, 1, 'hash-then-hash autovivification stores the value';
    # A *read* of an untouched sibling must not create anything.
    my $untouched = $h<zz>;
    nok $h<zz>:exists, 'reading an untouched sibling key does not autovivify it';
    is $h.elems, 1, 'the root still has exactly one key';
    nok $h<a><zz>:exists, 'reading an untouched nested sibling key does not autovivify it';
    is $h<a>.elems, 1, 'the nested hash still has exactly one key';
    my $deep = $h<q><r>;
    nok $h<q>:exists, 'a deep rvalue read creates no intermediate container';
    is $h.elems, 1, 'the root key count is unchanged after the deep read';
}

{
    my $a;
    $a[0]<k> = 1;
    is $a[0]<k>, 1, 'array-then-hash autovivification stores the value';
    is $a.elems, 1, 'array-then-hash autovivification creates one element';
}

{
    my $a;
    $a[0][1] = 1;
    is $a[0][1], 1, 'array-then-array autovivification stores the value';
    is $a[0].elems, 2, 'the inner array was resized to hold the index';
}

{
    my %g;
    %g<a><b> = 1;
    is %g<a><b>, 1, 'a %-sigil root autovivifies its nested hash';
    nok %g<zz>:exists, 'an untouched sibling of a %-sigil root does not exist';
    is %g.elems, 1, 'the %-sigil root has exactly one key';
}

# --- $*COLLATION -----------------------------------------------------------
# rakudo declares one process-wide mutable Collation; `.set` mutates it and
# every later read (including `coll`) observes the change.

is $*COLLATION.^name, 'Collation', '$*COLLATION is a Collation instance';
is $*COLLATION.primary, 1, '$*COLLATION.primary defaults to 1';
is $*COLLATION.raku, 'Collation.new(collation-level => 85)',
    '$*COLLATION.raku matches rakudo';
is ('a' coll 'A'), Less, 'coll distinguishes case at the default collation level';

sub collation-tertiary-in-sub { $*COLLATION.tertiary }
sub coll-in-sub { 'a' coll 'A' }

$*COLLATION.set(:quaternary(False), :tertiary(False));
is $*COLLATION.tertiary, 0, '.set(:tertiary(False)) is visible on a later read';
is $*COLLATION.quaternary, 0, '.set(:quaternary(False)) is visible on a later read';
is $*COLLATION.primary, 1, '.set leaves the untouched levels alone';
is ('a' coll 'A'), Same, 'coll honours the new collation level';
is collation-tertiary-in-sub(), 0, '.set is visible inside a called sub';
is coll-in-sub(), Same, 'coll inside a called sub honours the new level';
for 1..1 { is ('a' coll 'A'), Same, 'coll inside a for body honours the new level' }

done-testing;
