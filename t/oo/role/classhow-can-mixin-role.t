use Test;

# `.can` already found methods contributed by a runtime-mixed-in role
# (`dispatch_mixin_method_call`), but `.^can` / `nqp::can` walked the class
# registry MRO keyed off the receiver's underlying class and never looked at
# the Mixin's roles at all -- so a `but`/`does` role method was visible to
# `.can` but invisible to `.^can` and `nqp::can`, even though real Raku agrees
# across all three. Verified against `raku` (see
# news/2026-08/test-assertion-trait-is-not-introspectable.md, which needs
# `nqp::can` to see a routine-level role mixin for the same reason).

plan 6;

use nqp;

role R { method zz(--> True) { } }

sub foo() { 1 }
my $y = &foo but R;

ok $y.can("zz").Bool, '.can sees a role mixed onto a Sub';
ok nqp::can($y, "zz"), 'nqp::can sees a role mixed onto a Sub';
ok $y.^can("zz"), '.^can sees a role mixed onto a Sub';

# Also true for an ordinary value mixin, not just a Sub.
my $x = 5 but R;
ok $x.can("zz").Bool, '.can sees a role mixed onto a plain value';
ok nqp::can($x, "zz"), 'nqp::can sees a role mixed onto a plain value';
ok $x.^can("zz"), '.^can sees a role mixed onto a plain value';

done-testing;
