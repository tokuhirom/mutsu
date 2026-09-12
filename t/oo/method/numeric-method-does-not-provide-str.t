use v6;
use Test;

# #8153: a class that defines a `Numeric` method (but does not `does
# Real`/`does Numeric`) got its `.Str` routed through that method instead of
# falling back to `Mu.Str`. rakudo's `Mu.Str` renders `TypeName<id>`; mutsu's
# own default instance repr is `TypeName()` (a pre-existing, unrelated
# divergence -- both are "the default object stringification", neither is
# the Numeric value). What matters here is that a bare `Numeric` method has
# no bearing on `.Str`, `~`, string interpolation, matching rakudo, while
# numeric-context coercion (`+`) still bridges through it.

plan 7;

class P { method Numeric { 7 } }

isnt P.new.Str, '7', 'a bare Numeric method does not provide .Str';
isnt ~P.new, '7', 'nor does it provide prefix ~ stringification';
isnt "$(P.new)", '7', 'nor string interpolation';
is P.new + 1, 8, 'but numeric-context coercion still bridges to it';

# A class that genuinely composes Real/Numeric (not just a same-named method)
# keeps the documented `.Str`/`.gist` bridging exception.
class Q does Real {
    method Bridge { 7 }
}
is Q.new.Str, '7', 'a class that does Real still bridges .Str to its number';
is ~Q.new, '7', 'and prefix ~';
is Q.new.gist, '7', 'and .gist';
