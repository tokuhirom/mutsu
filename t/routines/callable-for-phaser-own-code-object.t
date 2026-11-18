use Test;
use nqp;

# `Block.callable_for_phaser` hands out a code object of its own: binding the
# block's `$!do` (ParaSeq does it on its mapper clone for `FIRST`) does not
# change what the phaser callable runs (#18024: ParaSeq t/09 "LAST phaser
# fired only once with degree 3").

plan 4;

my @log;
my $b = { LAST @log.push('last'); NEXT @log.push('next'); @log.push("body $_"); $_ };
my $last = $b.callable_for_phaser('LAST');
my $next = $b.callable_for_phaser('NEXT');
nqp::bindattr($b, Code, '$!do', nqp::clone(nqp::getattr($b, Code, '$!do')));
$last();
is @log.join(','), 'last', 'the LAST callable runs only the LAST phaser';

@log = ();
$next();
is @log.join(','), 'next', 'the NEXT callable runs only the NEXT phaser';

nok $last === $b, 'a phaser callable is not the block';
@log = ();
my @r = (1, 2).map($b);
is @log.join(','), 'body 1,next,body 2,next,last', 'the block itself still runs its body';
