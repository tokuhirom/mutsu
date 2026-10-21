# A top-level `require M <...>` binds the listed symbols in the unit's
# lexical scope (ADR-8751, #14140), with no run-time alias in the env or the
# routine registry: routines declared in the unit see them, a listed type
# name is declared when the unit is compiled, and nothing else is imported.
use Test;
use lib 't/lib';

plan 8;

require RequireScopedImports <&rsi-f $rsi-v RsiC RSI-K>;

sub uses-them { rsi-f() ~ ' ' ~ $rsi-v ~ ' ' ~ RsiC.hi ~ ' ' ~ RSI-K }
is uses-them(), 'rsi-f rsi-v rsi-c 42', 'a routine of the unit sees the symbols';
is EVAL('rsi-f()'), 'rsi-f', 'EVAL in the unit sees the routine';
is MY::<&rsi-f>.name, 'rsi-f', 'the routine is a lexical of the unit';
ok $rsi-v.VAR ~~ Scalar, 'the variable is bound to its container';

sub typed(RsiC $c) { $c.hi }
is typed(RsiC.new), 'rsi-c', 'a listed type name is a parameter type';
my RsiC $o = RsiC.new;
is $o.hi, 'rsi-c', 'and a variable type';
is ::('RsiC').^name, 'RequireScopedImports::RsiC', 'indirect lookup finds the type';

nok ::('&rsi-unlisted').defined, 'an unlisted export is not imported';
