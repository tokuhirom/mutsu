use Test;

# A nested BEGIN is lifted into the unit prologue from every position, since
# the lift walks the tree through the exhaustive mutable visitor
# (ADR-10499): a phaser body, a C-style loop header, a parameter default, an
# operand of any operator, a trait argument. Each expectation was checked
# against rakudo: the BEGIN runs once, before the unit's mainline, even when
# its enclosing code never runs.

plan 13;

my @log;

sub enter-phaser { ENTER { BEGIN @log.push('enter') } }
ok @log.first('enter'), 'a BEGIN in an ENTER phaser body runs';

sub first-phaser { FIRST { BEGIN @log.push('first') } }
ok @log.first('first'), 'a BEGIN in a FIRST phaser body runs';

if False { loop (my $i = BEGIN { @log.push('loop-init'); 0 }; $i < 1; $i++) { } }
ok @log.first('loop-init'), 'a BEGIN in a loop header runs';

sub with-default($a = BEGIN { @log.push('default'); 7 }) { $a }
ok @log.first('default'), 'a BEGIN in a parameter default runs at compile time';
is with-default(), 7, '... and is the default\'s value';

my $pointy = -> $y = BEGIN { @log.push('pointy'); 2 } { $y };
ok @log.first('pointy'), 'a BEGIN in a pointy-block default runs';

if False { my $x = 1 max BEGIN { @log.push('max'); 2 } }
ok @log.first('max'), 'a BEGIN operand of an infix word operator runs';

if False { my $x is default(BEGIN { @log.push('trait'); 5 }) }
ok @log.first('trait'), 'a BEGIN in a trait argument runs';

if False { my @a; @a[0] = BEGIN { @log.push('index-assign'); 1 } }
ok @log.first('index-assign'), 'a BEGIN on the right of an element assignment runs';

my @order;
if False { say 1 R+ BEGIN { @order.push('a'); 1 }; BEGIN @order.push('b') }
is-deeply @order, ['a', 'b'], 'nested BEGINs run in source order';

sub sees-static {
    my $x = 5;
    my $c = -> $y = BEGIN { $x } { $y };
    $c()
}
is sees-static().raku, 'Any', 'a BEGIN in a default sees the inner lexical in its static state';

my @runs;
for ^2 { @runs.push(BEGIN { @log.push('value'); 3 } + 0) }
is-deeply @runs, [3, 3], 'a value-form BEGIN operand is a constant of its site';

sub compound { my $y = 1; $y += BEGIN { @log.push('compound'); 1 }; $y }
is @log.grep('compound').elems, 1, 'a BEGIN on the right of a compound assignment runs once';
