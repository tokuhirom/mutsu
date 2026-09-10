use v6;
use Test;

# Pin for the light named-call bind plan (call_compiled_function_light):
# precomputed Symbol/slot binding must preserve the string-scan path's
# semantics — param shadowing of same-named caller lexicals, sub-signature
# renames, alias binding, required-param X::AdHoc, closure capture of a named
# param, and last-pair-wins duplicate handling.

plan 12;

# A missing optional named param shadows a same-named caller lexical (it must
# NOT read through to the caller's $color).
my $color = "outer";
sub f(:$color) { $color // "unset" }
is f(), "unset", 'missing optional named param shadows caller lexical';
is f(:color<in>), "in", 'passed named param binds';
is $color, "outer", 'caller lexical untouched after the call';

# Repeated calls through the light-call cache keep the same binding behavior.
my @seen;
for ^3 { @seen.push(f()) }
is-deeply @seen, ["unset", "unset", "unset"], 'cached calls rebind fresh each time';

# sub_signature rename: :color(:$colour) — both keys match, inner name binds.
sub g(:color(:$colour)) { $colour }
is g(:color<c1>), "c1", 'rename matches the outer key';
is g(:colour<c2>), "c2", 'rename matches the inner key';

# Required named param: missing => X::AdHoc at runtime.
sub h(:$x!) { $x }
is h(:x(1)), 1, 'required named param binds';
my $err;
{ h(); CATCH { default { $err = $_ } } }
ok $err ~~ X::AdHoc, 'missing required named param throws X::AdHoc';

# A closure inside the body captures the named param by name.
sub k(:$a) { my $c = { $a * 2 }; $c() }
is k(:a(21)), 42, 'closure captures named param';

# Duplicate named args: the last one wins.
sub dup(:$v) { $v }
is dup(:v(1), :v(2)), 2, 'last duplicate named arg wins';

# A named param whose caller passes a variable (VarRef-wrapped arg).
sub via-var(:$n) { $n + 1 }
my $m = 41;
is via-var(:n($m)), 42, 'named arg from a variable binds through VarRef';

# Captured-outer write from a named-param sub persists in the caller.
my $acc = 0;
sub bump(:$by) { $acc += $by }
bump(:by(5)); bump(:by(7));
is $acc, 12, 'captured-outer writes persist across light named calls';
