use v6;
use Test;

# `.^roles` on a `but`-mixed value lists the mixed-in roles FIRST,
# most-recently-applied first, ahead of whatever the base type composes:
#
#     role A { }
#     (1 but A).^roles          # (A, Real, Numeric)
#     ((1 but A) but B).^roles  # (B, A, Real, Numeric)
#
# mutsu answered for the base type alone and never looked at the
# `__mutsu_role__{name}` markers the mixin carries, even though `.^name`,
# `.does` and `~~` all read them. Every expectation below was measured against
# rakudo 2026.07.

plan 14;

role A { }
role B { }
role P[::T] { }
class C does A { }

sub roles($x) { $x.^roles.map(*.^name).join(",") }

# --- the ticket's repro ------------------------------------------------
is roles(1 but A), 'A,Real,Numeric', 'a mixed-in role is listed, ahead of the base type\'s';
is roles("s" but A), 'A,Stringy', 'the same over a Str';
is roles(1 but P[Int]), 'P[Int],Real,Numeric', 'a parameterised role keeps its arguments';

# --- application order: most recent first ------------------------------
is roles((1 but A) but B), 'B,A,Real,Numeric', 'two roles come out most-recently-applied first';
is roles((1 but B) but A), 'A,B,Real,Numeric', 'and the other application order reverses them';
is roles(1 but (A, B)), 'B,A,Real,Numeric', 'a single `but` of two roles orders them the same way';

# --- a mixin over an instance that already composes roles --------------
is roles(C.new but B), 'B,A', 'a mixin over a class instance precedes the class\'s own roles';
is roles(C.new but A), 'A,A', 'and raku does NOT dedupe a role that is both';

# --- the anonymous role a `but <non-role>` composes ---------------------
like roles(1 but "x"), /^ '<anon|' \d+ '>,Real,Numeric' $/,
    'a `but <non-role>` lists its anonymous role';

# --- what must not move -------------------------------------------------
is roles(1), 'Real,Numeric', 'a plain value is unchanged';
is roles(C), 'A', 'a class type object is unchanged';
is roles(C.new), 'A', 'and a plain instance of it';
is roles(A), '', 'a role with no parents lists nothing';

# Role punning: `R.new` builds a Mixin over an Instance of R, and `.^name`
# deliberately reports plain `R` rather than `R+{R}` — `.^roles` must not
# start listing the punned role twice either.
role Pun { method m { 1 } }
is roles(Pun.new), 'Pun', 'a punned role instance lists itself once';
