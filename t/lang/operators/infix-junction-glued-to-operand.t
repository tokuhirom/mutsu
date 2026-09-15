use Test;

# `&` written GLUED to its left operand is the all-junction infix, never the
# `&name` sigil. A sigil opens a TERM, and a term cannot begin in the middle of
# a token, so `Int&Str` has no sigil reading at all -- while `f &g` (space
# before, none after) really is the listop `f` taking `&g` as an argument.
#
# mutsu refused the infix whenever an identifier character followed the `&`,
# which was the right guard for `f &g` and made every glued all-junction of two
# barewords a hard parse error: `Int&Str`, and `A&B` over an enum's values
# (#7954). `|` and `^` were never affected, because neither doubles as a sigil.

plan 16;

enum E <A B>;
sub g { 'g-result' }

# Glued, both operands barewords.
is (A&B).raku, 'all(E::A, E::B)', 'glued & between two enum values';
is (Int&Str).raku, 'all(Int, Str)', 'glued & between two type objects';
is (A&B&Int).raku, 'all(E::A, E::B, Int)', 'glued & chains';

# Glued after something that is not a bareword.
is (g()&g()).raku, 'all("g-result", "g-result")', 'glued & after a call';
my $x = 1;
is ($x&2).raku, 'all(1, 2)', 'glued & after a scalar';
my %h = a => 7;
is (%h<a>&2).raku, 'all(7, 2)', 'glued & after a subscript';

# Spaced forms are unchanged.
is (A & B).raku, 'all(E::A, E::B)', 'spaced & between two enum values';
is (A |B).raku, 'any(E::A, E::B)', 'a sigil-less | is unaffected by spacing';
is (A^B).raku, 'one(E::A, E::B)', 'glued ^ is unaffected';
is (A|B).raku, 'any(E::A, E::B)', 'glued | is unaffected';

# `&name` as a TERM still works everywhere a term is expected -- the guard that
# was narrowed exists for exactly this.
sub takes-one($x) { $x.^name }
is (takes-one &g), 'Sub', 'a listop takes &g as its argument when spaced';
sub calls(&c) { c() }
is (calls &g), 'g-result', 'a &-sigil argument binds a Callable parameter';
is (&g).^name, 'Sub', '&g in parentheses is the routine';
my @routines = (&g, &g);
is @routines.elems, 2, '&g as a list element';

# The junction really is one, not a coincidence of stringification.
isa-ok (A&B), Junction, 'the glued form produces a Junction';
ok (1&1) == 1, 'and it autothreads';
