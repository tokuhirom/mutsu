use lib 't/lib';
use Test;

# `$obj.^meth(...)` calls `$obj.HOW.meth($obj, ...)`: a user-defined metaclass
# receives the object itself, not its type object. Found via Red, whose
# `$obj.^populate-ids` / `^set-id` need a defined `Red::Model:D $model`
# (RedX::HashedPassword t/020-basic.t).
use UserHowHatCall;

plan 4;

userhowhat Thing { }

my $obj = Thing.new;
is $obj.^who-am-i, "instance", '.^meth on an instance passes the instance to a user HOW';
is Thing.^who-am-i, "type object", '.^meth on the type object passes the type object';
is $obj.^kind, "plain:D", 'multi metaclass method sees the instance';
is $obj.^kind(:with<x>), "str", 'named-arg multi candidate still chosen';
