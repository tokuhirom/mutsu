use v6;
use Test;

# Pin for todo/tickets/fast-binder-skips-too-many-positionals-check.md:
# passing more positional arguments than a fixed-arity (no slurpy/named)
# sub/block signature accepts used to be silently dropped on mutsu's "fast"
# binding paths (call_compiled_function_fast / call_compiled_function_
# positional_light in src/vm/, and the legacy `param_defs.is_empty()` branch
# of bind_function_args_values in src/runtime/types/binding_signature.rs for
# a plain pointy-block/WhateverCode signature) instead of raising raku's
# "Too many positionals passed; expected N arguments but got M". A named
# param or a `where` constraint already forced calls onto the general
# binder, where the check always fired correctly -- this file exercises the
# shapes that used to skip it.

plan 6;

# A single-positional-param sub (call_compiled_function_positional_light).
sub g($a) { "g:$a" }
my $g-err;
{ my @z = (1, 2); g(|@z); CATCH { default { $g-err = $_ } } }
ok $g-err.defined && $g-err.message.contains('Too many positionals'),
    'sub with one positional param rejects a surplus argument';

# A two-positional-param sub (same fast path, arity > 1).
sub g2($a, $b) { "g2:$a$b" }
my $g2-err;
{ my @z = (1, 2, 3); g2(|@z); CATCH { default { $g2-err = $_ } } }
ok $g2-err.defined && $g2-err.message.contains('Too many positionals'),
    'sub with two positional params rejects a surplus argument';

# A zero-param sub (call_compiled_function_fast's dedicated zero-arg cache).
sub h() { "h" }
my $h-err;
{ my @z = (1,); h(|@z); CATCH { default { $h-err = $_ } } }
ok $h-err.defined && $h-err.message.contains('Too many positionals'),
    'zero-param sub rejects a surplus argument';

# A pointy block with one named param (call_compiled_closure's legacy
# param_defs-empty binder branch).
my $blk = -> $a { "blk:$a" };
my $blk-err;
{ $blk(1, 2); CATCH { default { $blk-err = $_ } } }
ok $blk-err.defined && $blk-err.message.contains('Too many positionals'),
    'pointy block with one param rejects a surplus argument';

# A non-mutating WhateverCode (same legacy binder branch, params = ["_"]).
my $wc = * + 1;
is $wc(5), 6, 'WhateverCode binds its single placeholder normally';
my $wc-err;
{ $wc(5, 6); CATCH { default { $wc-err = $_ } } }
ok $wc-err.defined && $wc-err.message.contains('Too many positionals'),
    'WhateverCode rejects a surplus argument';
