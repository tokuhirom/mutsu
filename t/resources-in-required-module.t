use v6;
use lib 't/lib/ResCaller/lib', 't/lib/ResDist/lib', 't/lib/ResInner/lib';
use Test;

plan 3;

# `%?RESOURCES` is lexically tied to the compilation unit that contains the
# token, so a module's own mainline/`BEGIN` must see ITS distribution's
# resources — including when the module is pulled in by `require` from inside
# another module's routine, where the innermost call frame belongs to the
# caller. mutsu resolved `%?RESOURCES` against that caller's distribution, so
# `%?RESOURCES<greeting.txt>` was `Any` and the `BEGIN … .slurp` blew up with
# "No such method 'slurp' for invocant of type 'Any'" — wrapped, unhelpfully,
# as "An exception occurred while evaluating a CHECK". That is exactly what
# stopped HTTP::UserAgent from loading IO::Socket::SSL (via OpenSSL::NativeLib)
# for an https request.

use ResRequirer;

my $greeting;
lives-ok { $greeting = ResRequirer.load-greeting },
    'a module `require`d from inside another module\'s method loads';
is $greeting, 'hello from the ResInner resources',
    'its BEGIN-time %?RESOURCES resolved against its own distribution';

# The plain `use` path must keep working too.
lives-ok { EVAL 'use ResDist; ResDist.greeting' },
    '%?RESOURCES still resolves on the `use` path';
