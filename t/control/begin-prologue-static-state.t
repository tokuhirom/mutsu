use Test;
use lib $?FILE.IO.parent(2).add('lib');

# ADR-0134 slice 1: a unit's top-level BEGIN runs before any of the unit's
# run-time code, in source order with the declarations it can observe, and it
# sees lexicals in their static state -- declared, but with no run-time
# initializer applied yet. What it stores there is the lexical's starting value.

plan 12;

my $c = True;
my $seen-c;
BEGIN $seen-c = $c.raku;
is $seen-c, 'Any', 'a BEGIN does not see a run-time initializer that precedes it';

my @a = 9;
BEGIN @a.push(1);
is-deeply @a, [9], 'a run-time initializer overwrites what a BEGIN stored';

my @b;
BEGIN @b.push(1);
@b.push(2);
is-deeply @b, [1, 2], 'a declaration without initializer keeps what a BEGIN stored';

my @log;
@log.push('run');
BEGIN @log.push('begin');
is-deeply @log, ['begin', 'run'], 'a BEGIN runs before earlier run-time statements';

my $n = 0;
BEGIN $n++;
BEGIN $n++;
is $n, 0, 'the initializer runs after every BEGIN';

class PrologueClass { method m { 7 } }
my $m;
BEGIN $m = PrologueClass.m;
is $m, 7, 'a class declared earlier is composed when the BEGIN runs';

sub prologue-sub { 9 }
my $f;
BEGIN $f = prologue-sub();
is $f, 9, 'a sub declared earlier is callable from a BEGIN';

constant PK = 3;
my $k;
BEGIN $k = PK;
is $k, 3, 'a constant declared earlier is visible to a BEGIN';

is EVAL('my $x = 0; BEGIN { $x = 1 }; $x'), 0,
    'an EVAL unit gets the same prologue';

use BeginPrologueFixture;
is $BeginPrologueFixture::seen, 'Any', 'a module unit gets the same prologue';
is-deeply @BeginPrologueFixture::order, ['begin', 'run'],
    'a module BEGIN runs before the module mainline';

my $dir = $*TMPDIR.add("begin-prologue-{$*PID}");
$dir.mkdir;
my $file = $dir.child('begin-then-undeclared.raku');
$file.spurt('BEGIN say "begin"; no-such-routine-anywhere();');
my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
ok $out.contains('begin') && $err.contains('no-such-routine-anywhere'),
    'the BEGIN runs before the undeclared-routine check reports';
$file.unlink;
$dir.rmdir;
