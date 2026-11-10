use Test;

# `Block.has-phaser($name)` / `Block.has-phasers` (ParaSeq's duckmap asks a
# block which of FIRST/NEXT/LAST it carries).
plan 14;

my $plain = { $_ + 1 };
my $next  = { NEXT { my $x = 1 }; $_ };
my $first = { FIRST { my $x = 1 }; $_ };
my $last  = { LAST { my $x = 1 }; $_ };
my $enter = { ENTER { my $x = 1 }; $_ };
my $both  = { NEXT { my $x = 1 }; LAST { my $y = 2 }; $_ };

nok $plain.has-phasers, "a plain block has no phasers";
ok $next.has-phasers, "a block with NEXT has phasers";
ok $both.has-phasers, "a block with NEXT and LAST has phasers";

nok $plain.has-phaser($_), "plain: no $_" for <FIRST NEXT LAST>;
ok  $next.has-phaser('NEXT'), "NEXT";
nok $next.has-phaser('FIRST'), "a NEXT block has no FIRST";
ok  $first.has-phaser('FIRST'), "FIRST";
ok  $last.has-phaser('LAST'), "LAST";
ok  $enter.has-phaser('ENTER'), "ENTER";
ok  $both.has-phaser('NEXT') && $both.has-phaser('LAST'), "both";

nok {;}.has-phaser('BEGIN'), "BEGIN is not a block phaser";
nok {;}.has-phaser('bogus'), "an unknown name";
