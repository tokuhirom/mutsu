use v6;
use Test;

# A plain gather is a `Seq` whose body runs when it is read (ADR-11678). An
# `@` attribute owns its elements, so every way of filling one runs the gather:
# an initializer, a named argument to `.new`, an assignment inside a method
# (Text::CSV's `@!crange = $range.to_list`) and one through an `is rw`
# accessor. Expectations are Rakudo v2026.09's.

plan 6;

class Default { has @.c = gather { take 1; take 2 } }
is-deeply Default.new.c, [1, 2], 'an @ attribute initialized by a gather';

class Mapped { has @.c = (1, 2).map(* + 0) }
is-deeply Mapped.new.c, [1, 2], 'an @ attribute initialized by a .map';

class Named { has @.c }
is-deeply Named.new(c => gather { take 1; take 2 }).c, [1, 2],
    'a gather passed as a named argument to .new';

class Assigned {
    has @.c;
    method fill { @!c = gather { take (1..3).Slip; take 5 } }
}
my $a = Assigned.new;
$a.fill;
is-deeply $a.c, [1, 2, 3, 5], 'a gather assigned to @!c in a method';

class Rw { has @.c is rw }
my $r = Rw.new;
$r.c = gather { take 8 };
is-deeply $r.c, [8], 'a gather assigned through an is rw accessor';

class Scalar { has $.s }
is Scalar.new(s => gather { take 1; take 2 }).s.List, (1, 2),
    'a $ attribute keeps the gather as a Seq';
