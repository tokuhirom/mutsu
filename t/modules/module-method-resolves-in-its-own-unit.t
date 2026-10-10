use Test;
use lib 't/lib';

# A method body resolves routine names in the compilation unit it was written
# in, like a sub body does, whichever unit calls it (ADR-12529 phase 2).

use OwnUnitHelperConsumer;
use OwnUnitHelperProvider;
use OwnUnitCounterClass;

plan 5;

sub helper() { 'main helper' }

is OwnUnitHelperObj.new.via-method, 'provider helper',
    "a method calls its own module's private helper";
is OwnUnitHelperObj.new.via-closure, 'provider helper',
    'so does a closure the method creates';
is through-consumer(), 'provider helper / provider helper',
    "a caller module's same-named private helper does not win";
is own-helper(), 'consumer helper', 'the caller module still has its own';
my $c = OwnUnitCounter.new;
my $s = 0;
$s = $c.step($s) for ^50;
is $s, 50,
    "a method calls a routine its module imported, in a loop";
