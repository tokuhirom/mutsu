use v6;
use Test;

# Pin for #8353: a short call to a fixed-arity, all-plain-positional
# signature (no slurpy/named/`^`-placeholder) used to leave its trailing,
# un-supplied parameter(s) unbound instead of rejecting the call -- the body
# then read the parameter as an undeclared variable (`X::Undeclared`) rather
# than raku's own "Too few positionals passed; expected N argument(s) but got
# M" (`X::AdHoc`), raised from the bind itself. Mirrors
# too-many-positionals-fixed-arity.t's shapes, in the short direction.

plan 8;

# A single-positional-param pointy block: the legacy `param_defs.is_empty()`
# branch of bind_function_args_values (src/runtime/types/binding_signature.rs).
my $id = -> $a { $a };
my $id-err;
{ $id(); CATCH { default { $id-err = $_ } } }
ok $id-err.defined && $id-err.message.contains('Too few positionals'),
    'a single-param pointy block rejects a call with no arguments';
is $id-err.message, 'Too few positionals passed; expected 1 argument but got 0',
    'and the message matches raku exactly, singular "argument"';

# A two-positional-param pointy block: same branch, arity > 1, and both
# "nothing supplied" and "one short" must reject.
my $two = -> $a, $b { "$a$b" };
my $two-err;
{ $two(); CATCH { default { $two-err = $_ } } }
ok $two-err.defined && $two-err.message.contains('Too few positionals'),
    'a two-param pointy block rejects a call with no arguments';
is $two-err.message, 'Too few positionals passed; expected 2 arguments but got 0',
    'and the message pluralizes "arguments" for arity 2';
my $two-short-err;
{ $two(1); CATCH { default { $two-short-err = $_ } } }
is $two-short-err.message, 'Too few positionals passed; expected 2 arguments but got 1',
    'and reports the actual count for a one-short call';
is $two(1, 2), '12', 'a fully-supplied call still binds normally';

# A non-mutating WhateverCode (same legacy binder branch, params = ["_"]).
my $wc = * + 1;
is $wc(5), 6, 'WhateverCode binds its single placeholder normally';
my $wc-err;
{ $wc(); CATCH { default { $wc-err = $_ } } }
ok $wc-err.defined && $wc-err.message.contains('Too few positionals'),
    'WhateverCode rejects a call with no arguments';
