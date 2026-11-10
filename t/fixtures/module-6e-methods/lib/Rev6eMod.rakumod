use v6.e.PREVIEW;
unit module Rev6eMod;

sub t-snitch() is export { (my $x = 42).snitch(-> $v { }); "snitch ok" }
sub t-nomark() is export { "é".nomark }
sub t-stem() is export { "a/b.txt".IO.stem }
sub t-can() is export { (1, 2).can('snip').so }
sub t-snip() is export { (1, 2, 5).snip(* < 3).raku }
