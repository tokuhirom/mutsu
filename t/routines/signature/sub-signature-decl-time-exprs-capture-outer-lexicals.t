use Test;

# A `where` constraint or a default inside a sub-signature (`*@ ($x where
# ...)`, `[$a, $b = ...]`) closes over the lexicals visible where the routine
# is declared, as a top-level parameter's does: a write it makes reaches that
# variable, and a read is not answered by a same-named lexical of the caller
# (ADR-12529 phase 3).
plan 7;

my $count = 0;
sub counted(*@ ($x where { $count++; True })) { $x }
sub call-counted { counted(1) }
call-counted();
is $count, 1, 'a sub-signature where block writes the declaration\'s variable';
counted(2);
is $count, 2, '...once per call';

my @seen;
sub logged(*@ ($x where { @seen.push($x); True })) { }
logged(5);
is-deeply @seen, [5], 'a sub-signature where block pushes to the declaration\'s array';

my $lim = 10;
sub limited(*@ ($x where * < $lim)) { "ok $x" }
sub caller-with-own-lim { my $lim = 0; limited(5) }
is caller-with-own-lim(), 'ok 5', 'a sub-signature where reads the declaration\'s $lim, not the caller\'s';

my $fallback = 'outer';
sub defaulted([$a, $b = $fallback]) { $b }
sub caller-with-own-fallback { my $fallback = 'caller'; defaulted([1]) }
is caller-with-own-fallback(), 'outer', 'a sub-signature default reads the declaration\'s lexical';

{
    my $inner = 0;
    sub in-block(*@ ($x where { $inner += $x; True })) { }
    in-block(3);
    in-block(4);
    is $inner, 7, 'a block-declared sub\'s sub-signature where writes the block\'s variable';
}

sub nested([$a, [$p where { $count++; True }]]) { $p }
nested([1, [2]]);
is $count, 3, 'a where two sub-signatures deep is captured too';
