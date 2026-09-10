use v6;
use Test;

# `.bless` on a ROLE type object puns the role into a class, exactly like
# `.new` does. Cro's composer builds its service object as
# `$service-type.bless(:@components)` where the default service type is the
# `Cro::Service` ROLE — without the pun the instance carried the role's name
# but none of its methods (`.start` was "No such method").

plan 5;

role Service {
    has @.components is required;
    has $!started;

    method start(--> Str) { $!started = True; "started" }
    method running(--> Bool) { ?$!started }
}

my $svc = Service.bless(components => [1, 2, 3]);
is $svc.^name, 'Service', 'blessed role instance carries the role name';
is $svc.components.elems, 3, 'bless named args populate role attributes';
is $svc.start, 'started', 'role methods are callable on the blessed instance';
ok $svc.running, 'private role attributes work through role methods';
ok $svc ~~ Service, 'the blessed instance smartmatches the role';

done-testing;
