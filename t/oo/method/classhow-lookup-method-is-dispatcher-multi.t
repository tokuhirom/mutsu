use Test;

# ADR-0019 Phase F box F1/F2 (todo/tickets/classhow-lookup-returns-sub-not-
# method-instance.md): `.^lookup`/`.^find_method` return a `Sub`-shaped
# value, not the `Method` `Instance` `.^methods` builds, so `Method`-only
# accessors like `.is_dispatcher` and `.multi` used to fall into the
# callable-compose fallback and silently return a bogus
# `<composed-method:NAME>` callable instead of a real answer. Ground truth
# gathered against `raku` 2026-08-14:
#
#   - An ordinary (non-multi) method's lookup answers both False.
#   - A multi method's dispatcher-shaped lookup (what `.^lookup`/
#     `.^find_method` return for the family as a whole) answers
#     `is_dispatcher` True but `multi` falsy.
#   - Each individual `.candidates[N]` entry answers `is_dispatcher` False
#     but `multi` True.
#   - A submethod lookup answers both False, same as an ordinary method.

class Plain { method foo { 1 } }
my $plain = Plain.^lookup("foo");
nok $plain.is_dispatcher, 'non-multi method: is_dispatcher is False';
nok $plain.multi, 'non-multi method: multi is falsy';

class Sub1 { submethod boot { 1 } }
my $sub = Sub1.^lookup("boot");
nok $sub.is_dispatcher, 'submethod: is_dispatcher is False';
nok $sub.multi, 'submethod: multi is falsy';

class Multi1 {
    multi method bar(Int $x) { "int" }
    multi method bar(Str $x) { "str" }
}
my $dispatcher = Multi1.^lookup("bar");
ok $dispatcher.is_dispatcher, 'multi method dispatcher: is_dispatcher is True';
nok $dispatcher.multi, 'multi method dispatcher: multi is falsy';

my @candidates = $dispatcher.candidates;
is @candidates.elems, 2, 'multi method dispatcher exposes both candidates';
for @candidates -> $c {
    nok $c.is_dispatcher, 'multi candidate: is_dispatcher is False';
    ok $c.multi, 'multi candidate: multi is True';
}

done-testing;
