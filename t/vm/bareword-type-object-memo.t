use Test;

# A bareword term remembers the type object of its own spelling for one
# registry write generation (ADR-0121 D3, #9291), while no `env` binding
# shares its name. These pin the answers the memo must not change: names
# that are rebound without a declaration, and declarations that move the
# generation.

plan 14;

class P { has $.x }
class T { }

sub make($n) { my $o; $o = P.new(x => $_) for ^$n; $o }

# The remembered value is the type object itself, every time.
is make(3).x, 2, 'a bareword class invocant constructs that class';
is make(3).x, 2, 'and still does once remembered';

# A type capture binds its name per call. A class of the same name exists,
# and the first call binds the capture to that very class.
sub captured(::T $x) { T.^name }
is captured(T.new), 'T', 'a type capture bound to the same-named class';
is captured(42), 'Int', 'the same capture bound to another type';
is captured('s'), 'Str', 'and to a third';

# A role's type parameter is resolved per composition.
role R[::U] { method u { U.^name } }
class A does R[Int] { }
class B does R[Str] { }
is A.u, 'Int', 'a role type parameter, first composition';
is B.u, 'Str', 'the same method body, another composition';
is A.u, 'Int', 'and the first again';

# A sigilless binding of the same spelling in another routine does not reach
# a bareword site that has already remembered its class.
sub name-of-p { P.^name }
is name-of-p(), 'P', 'a class bareword in a routine';
sub shadowing(\P) { P }
is shadowing(42), 42, 'a sigilless parameter named like the class is the parameter';
is name-of-p(), 'P', 'the routine still names the class';

# A declaration moves the registry generation; a remembered site keeps
# answering right across it.
sub later-name { P.^name }
is later-name(), 'P', 'before a later declaration';
class Later { }
is later-name(), 'P', 'after a later declaration';

# Threads run the same compiled chunk under their own registry snapshot.
my @names = await (^4).map: { start { make(2).^name } };
is-deeply @names, [<P P P P>], 'the site answers the class from other threads';
