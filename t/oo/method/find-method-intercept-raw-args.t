use Test;
use nqp;

# A type whose `^find_method` answers every call: the found method's raw
# (`\o`) parameters alias the caller's variable, and `?$obj` asks `.Bool`.
plan 4;

class Tramp {
    method ^find_method(Mu \type, Str:D $name) {
        return my method is-generic(--> False) { } if $name eq 'is-generic';
        my method found(\SELF: \o, |a) {
            nqp::assign(o, 99) if nqp::iscont(o);
            1
        }
        my method bool(\SELF: |a) { False }
        $name eq 'Bool' ?? &bool !! &found
    }
}

my $x = 5;
nqp::create(Tramp).zork($x);
is $x, 99, 'an instance call aliases the raw argument';

my $y = 5;
Tramp.zork($y);
is $y, 99, 'a type-object call aliases the raw argument';

my $obj = nqp::create(Tramp);
nok ?$obj, '?$obj asks the user .Bool';
nok $obj.Bool, '.Bool is answered by find_method';
