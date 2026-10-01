use Test;

# From the Spanish distribution: loop-control words wrapped as `sub (|c) { last |c }`.
plan 6;

my &stop = sub (|c) { last |c };
my @seen;
for 1..5 -> $i { @seen.push($i); stop() if $i == 3 }
is @seen, [1, 2, 3], 'last |c with an empty capture is a plain last';

my &next-it = sub (|c) { next |c };
my @odd;
for 1..5 -> $i { next-it() if $i %% 2; @odd.push($i) }
is @odd, [1, 3, 5], 'next |c with an empty capture is a plain next';

my &again = sub (|c) { redo |c };
my $tries = 0;
for 1..2 -> $i { $tries++; again() if $tries == 1 }
is $tries, 3, 'redo |c with an empty capture is a plain redo';

my &cont = sub (|c) { proceed |c };
my @w;
given 5 {
    when 5 { @w.push('when'); cont(); @w.push('not reached') }
    default { @w.push('default') }
}
is @w, ['when', 'default'], 'proceed |c with an empty capture is a plain proceed';

my @s;
given 5 {
    when 5 { @s.push('when'); succeed |(); @s.push('not reached') }
    default { @s.push('default') }
}
is @s, ['when'], 'succeed |() leaves the given';

my $r = do for 1..3 { last |() };
is $r.elems, 0, 'last |() in a loop body parses';
