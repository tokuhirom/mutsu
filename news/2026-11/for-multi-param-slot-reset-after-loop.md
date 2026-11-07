# A multi-parameter `for` no longer leaves its parameters in their slots

`for <a b>.kv -> $i, $var { }` nested in another loop, at the top of a `unit class` file, left `$i` and
`$var` holding their last values in the local slots after the loop. When the class body ended, the
enclosing `unit class` published them as statics, and a later fresh `my $i;` in code calling into that
namespace read the stale value (Math::Symbolic's `expand` took the wrong branch). The loop now resets the
slot of a parameter that had no binding before it. Math::Symbolic's `expand` no longer dies at that line
(its remaining differences are in #17022).
