use Test;

# A role method composed into a mixin (`$obj does R`, AttrX::Mooish's
# `Attribute+{AttrX::Mooish::Attribute}`) has one deferral candidate: its
# call pushes the empty dispatch boundary without matching the expansion.
# Its deferral must still find no next candidate, and a real override must
# still defer. (A deferral from a mixed-in role method that leaks into an
# enclosing method's frame is #18363, pre-existing.)

plan 5;

role Tagged {
    method tag() { 'tagged' }
    method defer-out() { callsame() // 'no next' }
}

my $attr = (class { has $.x }).^attributes[0] but Tagged;
is $attr.tag, 'tagged', 'a role method mixed into an Attribute';
is $attr.defer-out, 'no next', 'its callsame finds no next candidate';
is $attr.name, '$!x', 'the Attribute methods still answer';

role Loud { method speak() { 'LOUD ' ~ callsame() } }
class Dog { method speak() { 'woof' } }
my $dog = Dog.new but Loud;
is $dog.speak, 'LOUD woof', 'a mixin override still defers to the class method';
is Dog.new.speak, 'woof', 'the class itself is unchanged';
