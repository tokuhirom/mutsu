use Test;

# Distilled from Terminal::UI's Frame.make-height-computer:
#   for @$heights.kv -> $i, $h is rw { $h = ... }
# A positional view (.kv/.values/.pairs) of an Array reached through a
# scalar deref (`@$a`, `$a.list`) hands out the element containers, so an
# `is rw` loop parameter writes the Array, exactly as `@a.kv` does.

plan 6;

my $a = [2, 3];
for @$a.kv -> $i, $h is rw { $h = 7 if $i == 0 }
is-deeply $a, [7, 3], '@$a.kv with is rw';

my $b = [2, 3];
for $b.list.kv -> $i, $h is rw { $h += 10 }
is-deeply $b, [12, 13], '$b.list.kv with is rw';

my $c = [2, 3];
for @$c.values -> $v is rw { $v *= 2 }
is-deeply $c, [4, 6], '@$c.values with is rw';

my $d = [2, :1fr];
for @$d.kv -> $i, $h is rw { $h = 5 if $h ~~ Pair }
is-deeply $d, [2, 5], 'conditional write over a Pair element';

my $e = [2, 3];
for @$e.pairs -> $p { $p.value = 9 }
is-deeply $e, [9, 9], '@$e.pairs value assignment';

my $f = [2, 3];
my @snap = @$f.kv;
@snap[1] = 99;
is-deeply $f, [2, 3], 'a copied .kv list does not alias the source';
