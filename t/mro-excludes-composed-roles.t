use v6;
use Test;

# `.^mro` lists a class's linearization; a `does`-composed role is NOT in it.
#
#     role R2 { }; class K does R2 { }
#     K.^mro     # raku: K, Any, Mu     mutsu: K, R2, Any, Mu
#
# mutsu keeps composed roles in a class's `parents`, because that list IS the
# method-resolution walk: a role's methods are found by walking it. Rakudo
# composes them INTO the class, so its MRO does not need them -- and exposes
# them through the `:roles` adverb instead. `K.^mro(:roles)` is `K, R2, Any, Mu`
# there, which is exactly what mutsu's plain `.^mro` was returning. So the fix
# is not a filter that hides the dispatch order from introspection behind
# rakudo's back: the split is rakudo's own.
#
# An `is Role` PUN stays in the MRO, in rakudo and here -- only the pure-`does`
# compositions are dropped, which is what `class_does_only_roles` records and
# what `.^mro_unhidden` has always used for the same distinction.
#
# Every expectation below was measured against rakudo 2026.07.

plan 19;

role R2 { }
role R3 { method f { 'r3' } }
role RA { }
role RB does RA { }
class K does R2 { }
class K2 does R2 does R3 { }
class Base does R2 { }
class Derived is Base { }
class KN does RB { }
class Plain { }
class Sub is Plain { }
class Pun is R2 { }

sub mro($t) { $t.^mro.map({ .^name }).join(",") }

# --- the plain MRO excludes composed roles -----------------------------
is mro(K), 'K,Any,Mu', 'a single composed role is not in the MRO';
is mro(K2), 'K2,Any,Mu', 'nor are two of them';
is mro(Derived), 'Derived,Base,Any,Mu', "nor a role composed by a PARENT class";
is mro(KN), 'KN,Any,Mu', 'nor a role composed into another role';

# --- and the `:roles` adverb is where they live ------------------------
is K.^mro(:roles).map({ .^name }).join(","), 'K,R2,Any,Mu',
    '.^mro(:roles) lists the composed role';
is KN.^mro(:roles).map({ .^name }).join(","), 'KN,RB,RA,Any,Mu',
    '... including one composed into another role';

# --- an `is Role` pun is NOT dropped -----------------------------------
is mro(Pun), 'Pun,R2,Any,Mu', 'an `is Role` pun stays in the MRO';

# --- a role that is ALSO the declared parent is a PUN, not a composition --
# rakudo has exactly one in its own vocabulary: `X::TooLateForREPR`'s parent
# AND composed role are both `X::Comp`, and it really is in the MRO there. It
# is the documented exception to "a marker role name never appears in a class's
# `.^mro`" (see t/exception-role-membership.t).
is X::TooLateForREPR.^mro.map({ .^name }).join(' '),
    'X::TooLateForREPR X::Comp Exception Any Mu',
    'a role that is also the declared parent stays in the MRO';
is X::TooLateForREPR.^mro_unhidden.map({ .^name }).join(' '),
    'X::TooLateForREPR X::Comp Exception Any Mu',
    '... and in .^mro_unhidden, which used to drop it';
is X::TooLateForREPR.^roles.map({ .^name }).join(' '), 'X::Comp',
    '... while .^roles still reports it as a role';

# --- plain inheritance is untouched ------------------------------------
is mro(Sub), 'Sub,Plain,Any,Mu', 'ordinary inheritance is unchanged';
is mro(Plain), 'Plain,Any,Mu', 'and a bare class still reaches Any and Mu';

# --- everything the MRO walk still has to answer -----------------------
# These are the reason the roles are in `parents` at all; the filter is on the
# introspection output only, so none of them may move.
is K.^roles.map({ .^name }).join(","), 'R2', '.^roles still reports the composed role';
is K2.f, 'r3', 'a role method is still found on the type object';
is K2.new.f, 'r3', '... and on an instance';
ok K.isa(Any), 'the class still isa Any';
ok K.isa(Mu), '... and Mu';
ok K ~~ R2, 'and it still smartmatches the role';
ok K.new ~~ R2, '... as an instance too';
