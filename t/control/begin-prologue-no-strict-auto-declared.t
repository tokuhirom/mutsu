use Test;

# ADR-0134 slice 2 (#10482): a BEGIN nested in a scope under `no strict` may use
# a variable nothing declares -- it is an auto-declared package variable -- and
# is still lifted to BEGIN time. It runs once, before the unit's run time,
# whether or not its routine ever does. Expected values are rakudo's.
#
# A name that code outside the BEGINs also mentions is lifted too: the lifted
# BEGIN runs in a block of its own, but the variable it sets is the package's,
# which outlives that block (#10622).

BEGIN plan 8;

my @log;

sub in-routine { no strict; BEGIN { $auto1 = 3; @log.push("routine $auto1") } }
sub in-block { { no strict; BEGIN { $auto2 = 4; @log.push("block $auto2") } } }
sub array-var { no strict; BEGIN { @auto3 = 1, 2; @log.push("array @auto3[]") } }
sub hash-var { no strict; BEGIN { %auto4 = a => 1; @log.push("hash %auto4<a>") } }
is @log.join('|'), 'routine 3|block 4|array 1 2|hash 1',
    'a BEGIN under `no strict` auto-declares, and runs though its routine never does';

my @twice;
sub first-user { no strict; BEGIN { $dup = 1; @twice.push("first $dup") } }
sub second-user { no strict; BEGIN { $dup = 2; @twice.push("second $dup") } }
is @twice.join('|'), 'first 1|second 2',
    'two BEGINs may auto-declare one name between them';

my @late;
my @read-log;
no strict;
sub unit-level { BEGIN { $auto5 = 6; @late.push("unit $auto5") } }
sub unit-level-block { { BEGIN { $auto6 = 7; @late.push("unit block $auto6") } } }
is @late.join('|'), 'unit 6|unit block 7',
    'a `no strict` at file scope covers the BEGINs of the routines after it';

is @log.elems + @twice.elems + @late.elems, 8, 'every one of those BEGINs ran exactly once';

sub reads-it { BEGIN { $shared = 1; @read-log.push("read $shared") }; $shared }
sub never-called { BEGIN { $other = 2; @read-log.push("after $other") } }
is @read-log.join('|'), 'read 1|after 2',
    'a BEGIN whose name code outside it reads is still lifted, and so are the later ones';
is reads-it(), 1, 'a routine that also reads the auto-declared name sees the BEGIN\'s value';
is reads-it(), 1, '... on every call';
$shared = 5;
is reads-it(), 5, 'the routine and the unit share the one package variable';
