use Test;

plan 16;

my @log;
my $x = 10;
my $b = {
    FIRST { @log.push("first") }
    NEXT { @log.push("next1") }
    NEXT { @log.push("next2") }
    LAST { $x++ }
    ENTER { @log.push("enter1") }
    ENTER { @log.push("enter2") }
    $_
};

my $next = $b.callable_for_phaser("NEXT");
isa-ok $next, Block, "callable_for_phaser answers a Block";
$next();
is @log.join(","), "next2,next1", "NEXT phasers run last-declared first";

@log = ();
$b.callable_for_phaser("ENTER")();
is @log.join(","), "enter1,enter2", "ENTER phasers run in declaration order";

@log = ();
$b.callable_for_phaser("FIRST")();
is @log.join(","), "first", "a single phaser is answered directly";

$b.callable_for_phaser("LAST")();
todo "a phaser's write to a captured scalar is not written through yet (#15473)", 1;
is $x, 11, "a phaser writes the block's captured lexicals";

nok $b.callable_for_phaser("KEEP").defined, "no such phaser: undefined";
nok $b.callable_for_phaser("nonsense").defined, "unknown phaser name: undefined";
nok { 1 }.callable_for_phaser("NEXT").defined, "a block without phasers: undefined";

my @next = $b.phasers("NEXT");
is @next.elems, 2, "phasers lists every phaser of the name";
isa-ok @next[0], Block, "each entry is a Block";
@log = ();
@next[0]();
@next[1]();
is @log.join(","), "next2,next1", "phasers come in run order";
is $b.phasers("FIRST").elems, 1, "one FIRST phaser";
is $b.phasers("PRE").elems, 0, "no PRE phasers";
is $b.phasers("nonsense").elems, 0, "unknown name: empty";

ok Block.^can("callable_for_phaser"), "Block.^can callable_for_phaser";
my $wc = *+1;
dies-ok { $wc.callable_for_phaser("NEXT") }, "a WhateverCode is not a Block";
