use Test;

# An `our @a` that a closure captured and a BEGIN-time effect wrote to must
# keep that write when a bare block ends (the block-exit refresh of `our`
# aliases used to put the stale package container back over the live one).

plan 4;

our @seen;
our %tally;
sub note-it { @seen.push(1); %tally<k> = 2 }
BEGIN { note-it() }
is @seen.raku, '[1]', 'BEGIN-time push is visible';
{ my $x = 1; }
is @seen.raku, '[1]', 'the write survives a bare block';
is %tally.raku, '{:k(2)}', 'so does a hash write';

our @phase;
multi sub trait_mod:<is>(Mu:U $target, :$flag!) { @phase.push($flag) }
{ class InBlock is flag(3) { } }
is @phase.raku, '[3]', 'a trait run in a nested block keeps its write';
