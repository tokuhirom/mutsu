use v6;
use Test;

# A method always carries an implicit `*%_` slurpy so a caller may pass named
# arguments the signature does not name. This must hold for methods that a class
# inherits from a role, not just methods declared directly in the class body.

plan 6;

# 1. A role-inherited method absorbs an unnamed extra named argument.
role R1 { method f($a) { "got $a" } }
class C1 does R1 { }
is C1.f(1, :extra), "got 1", "role method absorbs stray named arg via implicit slurpy";

# 2. The implicit %_ is readable inside a role-inherited method body.
role R2 { method g($a) { "$a/" ~ %_<k> } }
class C2 does R2 { }
is C2.g("x", :k<v>), "x/v", "role method body can read implicit named slurpy";

# 3. Multiple stray named args are all absorbed.
role R3 { method h() { "ok" } }
class C3 does R3 { }
is C3.h(:a, :b, :c), "ok", "role method absorbs several stray named args";

# 4. Slurpy positional plus a stray named arg (the zef search shape).
role R4 { method s(*@xs) { @xs.join(",") } }
class C4 does R4 { }
is C4.s(1, 2, :strict), "1,2", "role method with *@ slurpy still absorbs named arg";

# 5. An explicit named param still binds; %_ only catches the rest.
role R5 { method n(:$x) { "$x/" ~ %_<y> } }
class C5 does R5 { }
is C5.n(:x<A>, :y<B>), "A/B", "explicit named binds, implicit slurpy catches rest";

# 6. Direct class method still works (no regression).
class C6 { method f($a) { "got $a" } }
is C6.f(1, :extra), "got 1", "class-direct method still absorbs stray named arg";
