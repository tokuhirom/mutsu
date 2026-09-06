use v6;
use Test;

# A user `multi infix:<op>` joins the operator's CANDIDATE SET; it does not
# replace the operator. Rakudo's core set for the numeric family is
# `(\a, \b)`, `(Real, Real)`, `(Int:D, Int:D)`, `(Num:D, Num:D)`,
# `(Rational:D, ...)`, `(Complex:D, ...)` plus per-operator temporal/Range rows;
# `infix:<~>`'s is `Str:D`/`Cool:D`/`Any:D` mixes over a slurpy catch-all, and
# the string-comparison family is `(Str:D, Str:D)` / `(Blob:D, Blob:D)` over
# `(\a, \b)`. So a user candidate only wins where the core set covers the
# operands with its `Mu` catch-all alone -- an untyped user parameter is `Any`,
# narrower than `Mu` but wider than `Int:D` / `Str:D` / `Real`.
#
# Every expectation below was measured against rakudo. Each row runs in its own
# EVAL so the candidate sets of different rows cannot leak into each other.

plan 50;

my $U = 'multi infix:<+>($a, $b) is default { "USER" }; ';

# Declared outside the EVAL: mutsu's EVAL returns an `enum` declaration's own
# value rather than the block's last statement, so the enum cannot be declared
# inside the EVAL'd string (todo/tickets/eval-returns-enum-declaration-value.md).
enum E <A B>;

# --- the core typed candidates win over an untyped user candidate ------------
is EVAL($U ~ '1 + 2'), 3, 'Int + Int runs the core (Int:D, Int:D) candidate';
is EVAL('multi infix:<+>($a, $b) { "USER" }; 1 + 2'), 3,
    'no `is default` needed -- the core candidate is simply narrower';
is EVAL($U ~ '1.5 + 2.5'), 4, 'Rat + Rat runs the core (Rational:D, Rational:D) candidate';
is EVAL($U ~ '1e0 + 2e0'), 3, 'Num + Num runs the core (Num:D, Num:D) candidate';
is EVAL($U ~ '1 + 2e0'), 3, 'a mixed Int/Num pair runs the core (Real, Real) candidate';
is EVAL($U ~ 'True + 1'), 2, 'Bool + Int runs the core (Int:D, Int:D) candidate';
is EVAL($U ~ '<42> + 1'), 43, 'an allomorph runs the core (Int:D, Int:D) candidate';
is EVAL($U ~ '((1+2i) + 1).gist'), '2+2i', 'Complex + Int runs the core Complex candidate';
is EVAL($U ~ '((1..2) + 1).gist'), '2..3', 'Range + Int runs the core (Range:D, Real:D) candidate';
is EVAL($U ~ '(1 + 2/4).gist'), '1.5',
    'a mixed Int/Rat pair runs the core (Int:D, Rational:D) candidate';

# --- everything the core set only reaches with `(\a, \b)` goes to the user ---
is EVAL($U ~ '"a" + "b"'), 'USER', 'Str + Str reaches the user candidate';
is EVAL($U ~ '1 + "2"'), 'USER', 'a mixed Int/Str pair reaches the user candidate';
is EVAL($U ~ 'Any + Any'), 'USER', 'two Any type objects reach the user candidate';
is EVAL($U ~ '1 + Nil'), 'USER', 'a Nil operand reaches the user candidate';
is EVAL('class P1 { }; ' ~ $U ~ 'P1.new + P1.new'), 'USER',
    'two user-class instances reach the user candidate';

# --- ranking rows, Int + Int unless noted ------------------------------------
is EVAL('multi infix:<+>(Any $a, Any $b) { "USER" }; 1 + 2'), 3,
    'an explicit Any user candidate is no narrower than an untyped one';
is EVAL('multi infix:<+>(Mu $a, Mu $b) { "USER" }; 1 + 2'), 3,
    'an explicit Mu user candidate loses to the core (Int:D, Int:D)';
is EVAL('multi infix:<+>(Cool $a, Cool $b) { "USER" }; 1 + 2'), 3,
    'a wider Cool user candidate loses to the core (Int:D, Int:D)';
is EVAL('multi infix:<+>(Real $a, Real $b) { "USER" }; 1 + 2'), 3,
    'a Real user candidate loses to the narrower core (Int:D, Int:D)';
is EVAL('multi infix:<+>(Numeric $a, Numeric $b) { "USER" }; 1 + 2'), 3,
    'a Numeric user candidate loses to the core (Int:D, Int:D)';
is EVAL('multi infix:<+>($a where * > 0, $b) { "USER" }; 1 + 2'), 3,
    'a where-constrained but untyped user candidate still loses on nominal type';
is EVAL('multi infix:<+>(Str $a, Str $b) { "USER" }; "a" + "b"'), 'USER',
    'a matching Str user candidate wins where the core set has only (\a, \b)';
is EVAL('multi infix:<+>(Str $a, $b) { "USER" }; "a" + 1'), 'USER',
    'a half-typed Str user candidate wins for a Str/Int pair';
is EVAL('multi infix:<+>($a, Str $b) { "USER" }; 1 + "a"'), 'USER',
    'a half-typed user candidate on the right operand wins too';
is EVAL('multi infix:<+>(Rat $a, Rat $b) { "USER" }; 1.5 + 2.5'), 'USER',
    'Rat out-narrows the core (Rational:D, Rational:D) role candidate';
is EVAL('multi infix:<+>(Int $a, Num $b) { "USER" }; 1 + 2e0'), 'USER',
    'a mixed user candidate beats the core (Real, Real) -- there is no (Int:D, Num:D)';
is EVAL('multi infix:<+>(UInt $a, UInt $b) { "USER" }; 1 + 2'), 'USER',
    'the core subset UInt out-narrows the core (Int:D, Int:D) too';
is EVAL('subset Sm of Int where * < 10; multi infix:<+>(Sm $a, Sm $b) { "USER" }; 1 + 2'),
    'USER', 'a subset of Int out-narrows the core (Int:D, Int:D)';
is EVAL('multi infix:<+>(E $a, E $b) { "USER" }; A + B'), 'USER',
    'an enum type out-narrows the core (Int:D, Int:D) its values carry';

# --- a plain `sub` is a lexical shadow, not a candidate ----------------------
is EVAL('sub infix:<+>($a, $b) { "USER" }; 1 + 2'), 'USER',
    'a non-multi sub infix:<+> replaces the operator outright';

# --- the rest of the natively implemented family -----------------------------
is EVAL('multi infix:<->($a, $b) is default { "USER" }; 5 - 3'), 2, 'infix:<-> ranks too';
is EVAL('multi infix:<*>($a, $b) is default { "USER" }; 2 * 3'), 6, 'infix:<*> ranks too';
is EVAL('multi infix:</>($a, $b) is default { "USER" }; 6 / 3'), 2, 'infix:</> ranks too';
is EVAL('multi infix:<**>($a, $b) is default { "USER" }; 2 ** 3'), 8, 'infix:<**> ranks too';
is EVAL('multi infix:<%>($a, $b) is default { "USER" }; 7 % 3'), 1, 'infix:<%> ranks too';
is EVAL('multi infix:<~>($a, $b) is default { "USER" }; "a" ~ "b"'), 'ab',
    'infix:<~> runs the core (Str:D, Str:D) candidate';
is EVAL('multi infix:<==>($a, $b) is default { "USER" }; 1 == 1'), True,
    'infix:<==> runs the core (Int:D, Int:D) candidate';
is EVAL('multi infix:<eq>($a, $b) is default { "USER" }; "a" eq "a"'), True,
    'infix:<eq> runs the core (Str:D, Str:D) candidate';
is EVAL('multi infix:«<»($a, $b) is default { "USER" }; 1 < 2'), True,
    'infix:<< < >> runs the core (Int:D, Int:D) candidate';
is EVAL('multi infix:<cmp>($a, $b) is default { "USER" }; (1 cmp 2).Str'), 'Less',
    'infix:<cmp> runs a core candidate';

# --- the user candidate is still reachable for every one of them -------------
is EVAL('class P2 { }; multi infix:<%>(P2 $a, P2 $b) { "USER" }; P2.new % P2.new'), 'USER',
    'infix:<%> consults a matching user candidate';
is EVAL('class P3 { }; multi infix:<~>(P3 $a, P3 $b) { "USER" }; P3.new ~ P3.new'), 'USER',
    'infix:<~> consults a matching user candidate';
is EVAL('class P4 { }; multi infix:<eq>(P4 $a, P4 $b) { "USER" }; P4.new eq P4.new'), 'USER',
    'infix:<eq> consults a matching user candidate';
is EVAL('class P5 { }; multi infix:<cmp>(P5 $a, P5 $b) { "USER" }; P5.new cmp P5.new'), 'USER',
    'infix:<cmp> consults a matching user candidate';

# --- the derived forms inherit the base operator's decision ------------------
is EVAL($U ~ 'my $x = 1; $x += 2; $x'), 3, 'the derived += inherits the core win';
is EVAL($U ~ '([+] 1, 2, 3).Str'), '6', 'the derived reduce inherits the core win';
is EVAL($U ~ 'my @a = 1, 2; my @b = 3, 4; (@a >>+<< @b).Str'), '4 6',
    'the derived hyper inherits the core win';
is EVAL('class P6 { }; multi infix:<+>(P6 $a, P6 $b) { "USER" }; my $x = P6.new; $x += P6.new; $x'),
    'USER', 'a derived += still reaches a matching user candidate';

# --- deliberately deferred: an exact nominal tie is an ambiguity error -------
# rakudo refuses `multi infix:<+>(Int $a, Int $b)` for `1 + 2` with
# "Ambiguous call to 'infix:<+>(Int, Int)'", listing the core `(Int:D $a, Int:D $b)`
# alongside the user's. mutsu gives the tie to the core candidate (ADR-0071's
# rule) and quietly runs the native operator instead. Reproducing the error
# needs the core candidate to carry a renderable signature, which is
# alternative B of ADR-0071 (synthetic `FunctionDef`s) -- see that ADR's
# "Known remaining divergences".
todo 'an exact nominal tie should be X::Multi::Ambiguous, not a core win', 2;
dies-ok { EVAL('multi infix:<+>(Int $a, Int $b) { "USER" }; 1 + 2') },
    'a user (Int, Int) candidate ties with the core (Int:D, Int:D)';
dies-ok { EVAL('multi infix:<~>(Str $a, Str $b) { "USER" }; "a" ~ "b"') },
    'a user (Str, Str) candidate ties with the core (Str:D, Str:D)';

done-testing;
