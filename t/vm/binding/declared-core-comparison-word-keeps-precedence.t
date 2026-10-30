use Test;

# Pop (Pop::Point) and others declare `multi infix:<eq>` / `lt` / `ne` / ... to
# add a candidate to the core comparison operator. The declaration kept the
# core operator's precedence in rakudo, but mutsu took the word at the
# trait-less additive level, so `2 + 1 eq 4 - 1` read as `2 + (1 eq 4) - 1`.
# `unicmp`, `coll` and `o` also ignored the declared candidate when the call
# went through the core operator's own opcode.

plan 14;

class T { has $.v }
multi infix:<eq>(T \a, T \b) { a.v == b.v }
multi infix:<ne>(T \a, T \b) { a.v != b.v }
multi infix:<lt>(T \a, T \b) { a.v < b.v }
multi infix:<eqv>(T \a, T \b) { a.v == b.v }

my $p = T.new(v => 1);
my $q = T.new(v => 2);

ok  ($p eq T.new(v => 1)), 'the declared eq candidate is called';
ok  ($p ne $q), 'ne';
ok  ($p lt $q), 'lt';
ok  ($p eqv T.new(v => 1)), 'eqv';
is (2 + 1 eq 4 - 1), (3 eq 3), 'eq stays looser than + and - after a multi of eq';
is (1 + 1 lt 5 - 1), True, 'lt stays looser than + and -';
is (1 + 1 ne 3 - 1), False, 'ne stays looser than + and -';
is (1 + 1 eqv 2), True, 'eqv stays looser than +';
is (3 * 2 eq 6 && 1 + 1 eq 2), True, 'and tighter than &&';
is ("a" eq "a"), True, 'the core candidate still serves other operands';

multi infix:<unicmp>(T \a, T \b) { 'USER-unicmp' }
multi infix:<coll>(T \a, T \b) { 'USER-coll' }
multi infix:<o>(T \a, T \b) { 'USER-o' }
is ($p unicmp $q), 'USER-unicmp', 'unicmp tries the declared candidate';
is ($p coll $q), 'USER-coll', 'coll tries the declared candidate';
is ($p o $q), 'USER-o', 'o tries the declared candidate';
is (({ $_ + 1 } o { $_ * 2 })(3)), 7, 'o still composes callables';
