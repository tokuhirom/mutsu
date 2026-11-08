# No `use Test`: rakudo rejects a plain `sub trait_mod:<is>` next to the
# Test module's own handlers ("Redeclaration of symbol"), so TAP is printed by hand.
my @log;
sub trait_mod:<is>(Variable:D $v, :$logged!) { @log.push("trait") }
@log.push("first");
my $x is logged = 1;
@log.push("last");

say "1..2";
say (@log.join(',') eq 'trait,first,last' ?? "ok" !! "not ok")
    ~ " 1 - a plain (non-multi) sub trait_mod:<is> handles a variable trait at BEGIN time";
say ($x == 1 ?? "ok" !! "not ok") ~ " 2 - the declaration still initializes the variable";
