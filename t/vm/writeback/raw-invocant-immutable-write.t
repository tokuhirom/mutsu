use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# A raw invocant (`\S:`) binds the caller's LOCATION when there is one. When
# there is not -- an immutable `List` element, a literal, an expression result --
# raku refuses the body's write to it ("Cannot modify an immutable Int (1)")
# rather than dropping it. mutsu bound such an invocant by value and the write
# succeeded silently.

plan 12;

augment class Int { method mutsuImmutMut(\S:) { S = 7 } }

sub outcome(&c) { (try { c(); "lived" }) // $!.^name ~ ": " ~ $!.message }

# --- no location: the write is refused --------------------------------------

{
    my $l = (1, 2);
    is outcome({ $l[0].mutsuImmutMut }),
        'X::Assignment::RO: Cannot modify an immutable Int (1)',
        'a List element has no location, so the raw-invocant write is refused';
    is $l.raku, '$(1, 2)', '... and the list is unchanged';
}
{
    my $s = $(1, 2);
    is outcome({ $s[0].mutsuImmutMut }),
        'X::Assignment::RO: Cannot modify an immutable Int (1)',
        'an ItemList element is refused the same way';
}
is outcome({ 42.mutsuImmutMut }),
    'X::Assignment::RO: Cannot modify an immutable Int (42)',
    'a literal invocant is refused';
{
    my $a = 1;
    is outcome({ ($a + 1).mutsuImmutMut }),
        'X::Assignment::RO: Cannot modify an immutable Int (2)',
        'an expression result is refused';
}

# --- a location: the write goes through -------------------------------------

{
    my $v = 1;
    is outcome({ $v.mutsuImmutMut }), 'lived', 'a scalar variable IS a location';
    is $v, 7, '... and the write reaches it';
}
{
    my @arr = 1, 2;
    is outcome({ @arr[0].mutsuImmutMut }), 'lived', 'a mutable Array element is a location';
    is @arr.raku, '[7, 2]', '... and the write reaches it';
}

# --- the ordinary store path keeps its own refusals --------------------------

{
    my $m = (1, 2);
    isa-ok (try { $m[0] = 7; Nil } // $!), X::Assignment::RO,
        'a direct store to a List element still dies';
}
{
    my @n := (1, 2);
    isa-ok (try { @n[0] = 7; Nil } // $!), X::Assignment::RO,
        'a direct store through a bound List still dies';
}
isa-ok (try { (1, 2)[0] = 7; Nil } // $!), X::Assignment::RO,
    'a direct store to a literal List element still dies';
