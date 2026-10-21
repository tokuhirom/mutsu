use Test;

# An argument-less user trait on a class (`class C is flagged`) runs in the
# declaration phase too: its callback runs before the lexical initializer that
# precedes the class in the source. Checked against `raku` first.

plan 4;

my $flag = 'x';
multi sub trait_mod:<is>(Mu:U $target, :$flagged!) { $flag = 'trait' }
class Flagged is flagged { }
is $flag, 'x', 'the initializer overwrites what the trait stored';

my @log;
multi sub trait_mod:<is>(Mu:U $target, :$logged!) { @log.push($target.^name) }
class Logged is logged { }
is @log.join(','), 'Logged', 'the trait ran once, with the class';

my $count;
multi sub trait_mod:<is>(Mu:U $target, :$counted!) { $count++ }
class Counted is counted { }
class Counted2 is counted { }
is $count, 2, 'each declaration applies its trait once';

class Parent { }
class Child is Parent { }
ok Child.new ~~ Parent, 'a capitalized parent is still a parent';
