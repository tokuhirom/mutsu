use Test;

# `$.attr = v` inside a method assigns through the attribute's PUBLIC ACCESSOR,
# not to the attribute slot: raku evaluates `self.attr` and assigns to what it
# hands back. A non-`rw` scalar accessor hands back a bare value, so the
# assignment dies. mutsu used to compile `$.attr = v` as an ordinary named
# assignment to the variable `.attr` -- the accessor was called only to check
# that it exists, and its result discarded -- so the write landed on the
# attribute and `is rw` was never consulted at all.
#
# Scoped to the `$` sigil on purpose. Measured against raku v2026.07: for a
# non-`rw` `has @.a` / `has %.h`, `@.a = 7,8`, `@.a[0] = 99` and `%.h<k> = 99`
# all SUCCEED, because those accessors hand back the container itself and
# assigning into a container is a STORE, not a modification of an immutable
# value.

plan 12;

class RO {
    has $.x = 5;
    has @.a = (1, 2);
    has %.h = (k => 1);
    method simple      { $.x = 9 }
    method simple-expr { my $r = ($.x = 9); $r }
    method peek        { $!x }
    method arr-assign  { @.a = 7, 8; @!a }
    method arr-elem    { @.a[0] = 99; @!a }
    method arr-push    { @.a.push(3); @!a }
    method hash-elem   { %.h<k> = 99; %!h<k> }
}

class RW {
    has $.y is rw = 5;
    method simple   { $.y = 9; $!y }
    method compound { $.y *= 2; $!y }
    method selfform { self.y = 7; $!y }
}

my $ro = RO.new;
throws-like { $ro.simple }, X::Assignment::RO,
    '$.x = v on a non-rw scalar accessor throws';
is $ro.peek, 5, 'and the attribute is untouched';

throws-like { RO.new.simple-expr }, X::Assignment::RO,
    'the same in expression position';

# raku spells this "Cannot modify an immutable Int (5)" -- the accessor's
# return type and value, not the accessor's name.
my $msg = '';
{ RO.new.simple; CATCH { default { $msg = .Str } } }
like $msg, /'Cannot modify an immutable Int (5)'/,
    'the message names the immutable value, as raku does';

# A container accessor is NOT read-only in this sense, even without `is rw`.
is RO.new.arr-assign, [7, 8], '@.a = ... succeeds without is rw';
is RO.new.arr-elem, [99, 2], '@.a[0] = ... succeeds without is rw';
is RO.new.arr-push, [1, 2, 3], '@.a.push succeeds without is rw';
is RO.new.hash-elem, 99, '%.h<k> = ... succeeds without is rw';

# `is rw` is unaffected in every form.
is RW.new.simple, 9, '$.y = v mutates an is-rw attribute';
is RW.new.compound, 10, '$.y *= 2 mutates an is-rw attribute';
is RW.new.selfform, 7, 'self.y = v mutates an is-rw attribute';

# A private twigil write is always allowed.
class Priv { has $.z = 5; method bump { $!z = 9; $!z } }
is Priv.new.bump, 9, '$!z = v is unaffected';
