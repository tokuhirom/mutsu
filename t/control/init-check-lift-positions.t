use Test;

# An INIT or CHECK phaser runs once, before the mainline, wherever it is
# written. The phaser lift reaches every position since it walks the tree
# through the exhaustive mutable visitor (ADR-10499); each expectation was
# checked against rakudo.

plan 9;

my @log;
@log.push('main');

sub before-main($tag) { (@log.first($tag, :k) // Inf) < @log.first('main', :k) }

class C1 { method m { INIT @log.push('method') } }
# TODO: rakudo runs it before the mainline too; mutsu runs a class body's
# INIT when the class body runs (#10552).
ok @log.first('method'), 'INIT in a method body runs without the method being called';

sub enter-phaser { ENTER { INIT @log.push('enter') } }
ok before-main('enter'), 'INIT in an ENTER phaser body';

sub leave-phaser { LEAVE { INIT @log.push('leave') } }
ok before-main('leave'), 'INIT in a LEAVE phaser body';

my @a;
@a[0] = { INIT @log.push('index-assign'); 1 };
ok before-main('index-assign'), 'INIT in a closure on the right of an element assignment';

my @fed = (1, 2) ==> map { INIT @log.push('feed'); $_ };
ok before-main('feed'), 'INIT in a closure in a feed';
is-deeply @fed, [1, 2], '... and the feed still runs';

my $x = 5;
$x += INIT { @log.push('compound'); 3 };
ok before-main('compound'), 'INIT on the right of a compound assignment';
is $x, 8, '... runs once and is the operand\'s value';
is @log.grep('compound').elems, 1, '... exactly once';
