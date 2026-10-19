use Test;

# A plain `sub` in a role body is the role's own routine. Composing the role
# into a class used to register a second copy under the composer's package
# and apply the sub's traits to that copy, so a trait that wraps the routine
# (as NativeCall's `is native` binds its body) answered a call from a
# composed method with the declared body (#13645). Checked against rakudo.

plan 7;

multi trait_mod:<is>(Routine $r, :$wrapped!) { $r.wrap(-> |c { 'wrapped' }) }

role R {
    method bare  { f() }
    method amp   { &f() }
    sub f is wrapped { 'plain' }
}
class K does R { }
is K.new.bare, 'wrapped', 'a composed method calling the sub by name runs the wrapper';
is K.new.amp, 'wrapped', 'and through its code object';

class C { method m { h() }; sub h is wrapped { 'plain' } }
is C.new.m, 'wrapped', 'a class body sub keeps its trait too';

role Q { my $v = 'role lexical'; method m { g() }; sub g { $v } }
class KQ does Q { }
is KQ.new.m, 'role lexical', 'a role body sub still sees the role body lexicals';
dies-ok { EVAL 'g()' }, 'and is not callable outside the role';

role P { submethod BUILD { $*built = p() }; method m { p() }; sub p is wrapped { 'plain' } }
my $*built;
my $punned = P.new;
is $*built, 'wrapped', 'a punned role\'s BUILD runs the wrapper';
is $punned.m, 'wrapped', 'and so does its method';
