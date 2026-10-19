unit module UseInBlockRerunTypes;

class UibrtThing is export { has $.n = 1; method twice { $!n * 2 } }
subset UibrtSmall of Int is export where * < 10;
enum UibrtColor is export <UibrtRed UibrtGreen>;

sub uibrt-make($n) is export { UibrtThing.new(:$n) }
