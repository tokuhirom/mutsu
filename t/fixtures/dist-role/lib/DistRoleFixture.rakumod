unit module DistRoleFixture;

# A role whose methods read $?DISTRIBUTION. When the role is composed into a class
# and one of its methods is dispatched (compiled on demand, after the module has
# finished loading), $?DISTRIBUTION must still resolve to this module's
# distribution, not Nil. Mirrors zef's `Zef::Pluggable` role, whose `!try-load`
# reads `$?DISTRIBUTION.meta<provides>`.
role HasDist is export {
    method dist-defined { $?DISTRIBUTION.defined }
    method provides-has(Str $k) { so $?DISTRIBUTION.meta<provides>{$k}:exists }
}

class Consumer does HasDist is export { }

sub make-consumer() is export { Consumer.new }

# Also exercise a plain exported sub (not a role method) reading $?DISTRIBUTION.
sub sub-dist-defined() is export { $?DISTRIBUTION.defined }
