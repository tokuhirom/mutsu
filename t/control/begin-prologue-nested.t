use Test;

# ADR-0134 slice 2: a BEGIN nested in a routine, closure, loop or block runs
# once, in the unit's BEGIN prologue, whether or not the enclosing code ever
# runs. It sees the enclosing scopes' lexicals in their static state, and a
# value-form BEGIN's value is a constant of its site.

plan 16;

my @log;

sub never-called { BEGIN @log.push('in-uncalled-sub') }
ok @log.first('in-uncalled-sub'), 'a BEGIN in a sub that is never called runs';

my $loop-runs;
for 1..3 { BEGIN $loop-runs++ }
is $loop-runs, 1, 'a BEGIN in a loop body runs once';

if False { BEGIN @log.push('in-dead-branch') }
ok @log.first('in-dead-branch'), 'a BEGIN in a branch never taken runs';

my $block = { BEGIN 42 };
is $block(), 42, 'a BEGIN that ends a block is the block\'s value';

my $outer-seen;
{
    my $x = 2;
    BEGIN $outer-seen = $x.raku;
}
is $outer-seen, 'Any', 'an inner lexical is in its static state at BEGIN time';

my $shadow = 1;
my $shadow-seen;
sub shadowing { my $shadow = 5; BEGIN $shadow-seen = $shadow.raku }
is $shadow-seen, 'Any', 'an inner declaration shadows the unit-level one at BEGIN time';

sub static-start { my $v; BEGIN $v = 5; $v++ }
is static-start(), 5, 'an inner lexical starts from what a BEGIN stored (1)';
is static-start(), 5, 'an inner lexical starts from what a BEGIN stored (2)';

for ^2 {
    my $iter;
    BEGIN $iter = 'static';
    is $iter, 'static', 'each loop iteration starts from the static value';
    $iter = 'changed';
}

{
    is early-read(), 3, 'a sub called before the declaration sees the static value';
    my $a; BEGIN { $a = 3 };
    sub early-read { $a }
}

my $unit = True;
my $unit-seen;
sub reads-unit { BEGIN $unit-seen = $unit.raku }
is $unit-seen, 'Any', 'a nested BEGIN sees unit lexicals in their static state';

sub value-form { my $v = BEGIN 3; $v }
is value-form(), 3, 'a value-form BEGIN in a sub keeps its value';

my @order;
@order.push('run');
my $t = BEGIN { @order.push('begin'); 5 };
is-deeply @order, ['begin', 'run'], 'a unit-level value-form BEGIN runs in the prologue';

{
    my $foo = 42;
    BEGIN { $foo = 23 }
    constant timecheck = $foo;
    is timecheck, 23, 'a nested constant reads the static value a BEGIN stored';
}

my $count;
my &counted = { BEGIN $count++; 1 };
counted(); counted();
is $count, 1, 'a BEGIN in a closure does not run per call';
