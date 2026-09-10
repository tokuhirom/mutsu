use Test;

# ADR-0019 Phase F box F1 mechanism slice (todo/deep/
# adr0019-f1-f2-introspection-canonical-source.md "Decision (2026-08-14)"):
# the `Method` `Instance` objects `.^methods`/`.^method_table` build never
# set a `.package` attribute at all, so it always read `Nil` regardless of
# receiver. Ground truth gathered against `raku` 2026-08-14:
#
#   - A user-declared class method's `.package` is exactly its declaring
#     class.
#   - A runtime-mixed-in role method's `.package` is exactly the role.
#   - A multi method's *dispatcher*-shaped entry (what `.^methods` reports
#     for the family as a whole) has `.package` `(Dummy)` in real Rakudo, a
#     synthetic internal type mutsu does not model. mutsu deliberately leaves
#     it unset (Nil) here rather than guess a wrong concrete class -- this
#     is a known, accepted divergence from `raku`, not a claim of parity.
#   - Each individual `.candidates[N]` entry's `.package` is exactly the
#     declaring class, same as a non-multi method.
#
# Native/built-in methods are NOT pinned here: their true declaring type is
# not mechanically derivable from mutsu's per-type catalog (e.g. `Str.uc`'s
# real `.package` is `Cool`, not `Str` -- see the linked design doc's
# fidelity-slice discussion), so this file only covers the user-declared
# side the mechanism slice makes exact.

class Plain { method foo { 1 } }
is Plain.^lookup("foo").package.gist, '(Plain)',
    'Sub-shaped .^lookup result already answered .package correctly (unaffected baseline)';

my @plain-methods = Plain.^methods;
my $foo-method = @plain-methods.first(*.name eq 'foo');
is $foo-method.package.gist, '(Plain)',
    '.^methods Method-Instance package is the declaring class';

my %table = Plain.^method_table;
is %table<foo>.package.gist, '(Plain)',
    '.^method_table Method-Instance package is the declaring class';

role R { method bar { 1 } }
my $mixed = 5 but R;
my $bar-method = $mixed.^methods.first(*.name eq 'bar');
is $bar-method.package.gist, '(R)',
    'runtime-mixed-in role method package is the role';

class Multi1 {
    multi method baz(Int $x) { "int" }
    multi method baz(Str $x) { "str" }
}
my $dispatcher = Multi1.^methods.first(*.name eq 'baz');
is $dispatcher.package.gist, 'Nil',
    'multi dispatcher package stays unset rather than guessed';
for $dispatcher.candidates -> $c {
    is $c.package.gist, '(Multi1)',
        'multi candidate package is the declaring class';
}

done-testing;
