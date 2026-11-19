# A class under the `SmileyVis` namespace whose routines name it with a type
# smiley (#18254).
class SmileyVis::Thing { }

sub is-defined-thing($t) is export { $t ~~ SmileyVis::Thing:D }
sub is-thing-type($t) is export { $t ~~ SmileyVis::Thing:U }
sub all-things(@t) is export { so @t.all ~~ SmileyVis::Thing:D }
