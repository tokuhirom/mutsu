use Test;

plan 8;

sub msg(&c) { my $m; try { c(); CATCH { default { $m = .message } } }; $m }

# A built-in multi method that fails to dispatch lists rakudo's candidates.
my $skip = msg({ (1..12).Seq.skip(2, 3, 4) });
my @l = $skip.lines;
is @l[0], 'Cannot resolve caller skip(Seq:D: Int:D, Int:D, Int:D); none of these signatures matches:', 'skip: header';
is @l.elems, 5, 'skip: four candidates';
is @l[1].trim, '($:: *%_)', 'skip: first candidate';
is @l[4].trim, '($:: Int(Any) $n, *%_)', 'skip: last candidate';

my $lines = msg({ Str.lines });
@l = $lines.lines;
is @l[0], 'Cannot resolve caller lines(Str:U: ); none of these signatures matches:', 'lines: header';
is @l.elems, 9, 'lines: eight candidates';
is @l[1].trim, '(Cool:D $:: *%_)', 'lines: Cool candidate first';
is @l[8].trim, '(Str:D $:: Bool :$chomp!, *%_ --> Seq:D)', 'lines: Str candidate last';
