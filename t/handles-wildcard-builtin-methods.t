use v6;
use Test;

plan 17;

# builtin interception (attribute-based)
class FwdAttr { has $.t handles * = 'hello'; }
is FwdAttr.new.uc, 'HELLO', 'attribute handles *: .uc delegates';
is FwdAttr.new.subst('h', 'j'), 'jello', 'attribute handles *: n-arg .subst delegates';

# builtin interception (method-based)
class FwdMeth { method inner() handles * { 'hello' } }
is FwdMeth.new.uc, 'HELLO', 'method handles *: .uc delegates';
is FwdMeth.new.flip, 'olleh', 'method handles *: .flip delegates';

# method-based wildcard with a non-builtin name (bug 2)
class Delegate { method greet() { 'hi-from-D' } }
class FwdMeth2 { method inner() handles * { Delegate.new } }
is FwdMeth2.new.greet, 'hi-from-D', 'method handles *: custom method delegates';

# variable receiver exercises the VM mut-op path
my $o = FwdAttr.new;
is $o.uc, 'HELLO', 'variable receiver delegates too';

# real methods always win
class OwnWins { method inner() handles * { 'hello' }; method uc() { 'OWN' } }
is OwnWins.new.uc, 'OWN', 'own method beats handles *';
class P { method uc() { 'PARENT' } }
class InhWins is P { method inner() handles * { 'hello' } }
is InhWins.new.uc, 'PARENT', 'inherited method beats handles *';
role R { method uc() { 'ROLE' } }
class RoleWins does R { method inner() handles * { 'hello' } }
is RoleWins.new.uc, 'ROLE', 'role method beats handles *';

# delegation is inherited
class SubFwd is FwdMeth { }
is SubFwd.new.uc, 'HELLO', 'handles * is inherited by subclasses';

# explicit handles list also intercepts a builtin
class Expl { has $.t = 'hello'; method x() handles <uc> { $!t } }
is Expl.new.uc, 'HELLO', 'explicit handles <uc> delegates';

# ordering vs FALLBACK
class Both { method inner() handles * { 'hello' }; method FALLBACK($n, |c) { "FB:$n" } }
is Both.new.uc, 'HELLO', 'handles * beats FALLBACK';
class FbOnly { method FALLBACK($name, |c) { "FB:$name" } }
is FbOnly.new.uc, 'FB:uc', 'FALLBACK alone intercepts .uc';
class Bare { }
class FwdFb { method inner() handles * { Bare.new }; method FALLBACK($n, |c) { "FB:$n" } }
# These two depend on `Bare.new.uc` itself dying with "No such method" (Bare
# is a plain Any-derived class, not Cool-derived, per raku semantics) so the
# wildcard block's `Err(_) => continue` falls through to FALLBACK / the final
# error. Fixed by ADR-0051 P4 (docs/adr/0051-type-ancestry-has-one-oracle-and-
# an-unresolved-method-throws.md): the native fast path now checks
# `e2_native_method_exists` before answering a Cool-only builtin method
# (`.uc`, ...) for an `Instance` receiver, so `Bare.new.uc` dies with
# "No such method" instead of stringifying to "BARE()".
is FwdFb.new.uc, 'FB:uc', 'FALLBACK fires when the delegate cannot handle';

# missing on delegate, no FALLBACK: dies naming the method
class FwdMiss { method inner() handles * { Bare.new } }
throws-like { FwdMiss.new.uc }, Exception, message => /uc/,
    'missing on delegate dies with no-such-method for uc';

# Any/Mu methods are NOT intercepted
class FwdList { has $.t handles * = (1, 2, 3); }
is FwdList.new.elems, 1, '.elems resolves on Any, not delegated';
ok FwdList.new.gist.contains('FwdList'), '.gist stays Mu.gist, not delegated';
