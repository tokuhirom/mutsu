use v6;
use Test;

# A bare `callsame` / `nextsame` / `nextcallee` / `lastcall` term is resolved
# by the bareword chain only after every declaration of its name had first
# refusal, and its `GetBareWord` site remembers that route (per registry
# generation, re-checking the env). Every call below runs in a loop so the
# later iterations take the remembered route; each must answer what the full
# resolution answers, also where a binding of the same name takes over.

plan 7;

class Base { method greet($x) { "base:$x" } }
class Kid is Base {
    method greet($x) { "kid+" ~ callsame }
}
is (^3).map({ Kid.new.greet($_) }).join(' '), 'kid+base:0 kid+base:1 kid+base:2',
    'a bare callsame on every call';

class Kid2 is Base { method greet($x) { nextsame } }
is (^3).map({ Kid2.new.greet($_) }).join(' '), 'base:0 base:1 base:2',
    'a bare nextsame on every call';

sub plain($x) { "plain:$x" }
&plain.wrap(sub ($x) { my &next = nextcallee; "w(" ~ next($x) ~ ")" });
is (^3).map({ plain($_) }).join(' '), 'w(plain:0) w(plain:1) w(plain:2)',
    'a bare nextcallee in a wrapper on every call';

class Kid3 is Base { method greet($x) { lastcall; "last" } }
is (^3).map({ Kid3.new.greet($_) }).join(' '), 'last last last', 'a bare lastcall';

class Methods {
    method m($x) is rw { $x }
}
my $count = 0;
Methods.^find_method('m').wrap(method ($x) { $count++; my &o = nextcallee; o(self, $x) });
is (^4).map({ Methods.new.m($_) }).join(','), '0,1,2,3',
    'a wrapped method reaching the original through nextcallee';
is $count, 4, 'the wrapper ran on every call';

# A lexical binding of the same name takes over from the route: the
# resolution chain answers it ahead of the call-chain arm.
{
    my @seen;
    sub probe() {
        my \nextcallee = 'shadowed';
        nextcallee
    }
    @seen.push(probe()) for ^3;
    is @seen.join(','), 'shadowed,shadowed,shadowed', 'a sigilless lexical shadows the term';
}
