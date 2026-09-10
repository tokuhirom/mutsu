use Test;

# §B (#3658): the compiled-method fast path must validate arity like the tree-walk
# did. A call carrying a named argument (a Pair) or one that leaves a required
# positional unfilled is routed to the full path, which dies with the proper
# arity error — the fast path's index loop would otherwise mis-bind a named Pair
# to a positional param or silently bind a missing required positional to Nil.
# (Exposed once empty-body methods like `method CALL-ME($x){}` began compiling.)

plan 8;

# Missing required positional dies.
class M1 { method foo($x) { } }
dies-ok { M1.new.foo }, 'missing required positional dies (empty body)';

class M2 { method bar($x) { 1 } }
dies-ok { M2.new.bar }, 'missing required positional dies (non-empty body)';

# A named arg must not satisfy a required positional.
class M3 { method baz($x) { } }
dies-ok { M3.new.baz(:a) }, 'named arg does not fill a required positional (dies)';

# CALL-ME via postfix () with wrong arity dies.
class C1 { method CALL-ME(C1:U: $x) { } }
dies-ok { C1(:a) }, 'CALL-ME with only a named arg dies';
dies-ok { C1.() }, 'CALL-ME with no args dies (required positional missing)';

# Correct calls still work (fast path intact).
class OK1 { method add($a, $b) { $a + $b } }
is OK1.new.add(2, 3), 5, 'correct positional call still works (fast path)';

# Optional positional may be omitted without dying.
class OK2 { method greet($name?) { $name.defined ?? "hi $name" !! 'hi' } }
is OK2.new.greet, 'hi', 'optional positional may be omitted';

# Named arg lands in the implicit %_ when there is a required positional supplied.
class OK3 { method m($x) { %_<extra> // 'none' } }
is OK3.new.m(1, :extra<yes>), 'yes', 'named arg goes to implicit %_ alongside a positional';
