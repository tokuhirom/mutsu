use Test;

plan 5;

# `run_role_submethod` (src/runtime/types/roles.rs) seeds a role's BUILD/TWEAK
# submethod's private-attribute env vars before running the body on a
# does/but-mixed plain (non-Instance) value. The seed/readback key must carry
# the attribute's declared sigil (`@!attr`/`%!attr`), matching what the
# compiled body actually reads/writes for an array/hash attribute — a
# scalar-only key (`!attr`) silently no-ops `@!attr.push(...)` /
# `%!attr<k> = v`. See t/role-submethod-runtime-does-compiled.t for the
# scalar-attribute sibling cases. Every expectation below was checked against
# Rakudo v2026.06.

role BuildsHash { has %.h; submethod BUILD { %!h<a> = 1 } }
my $v = 0;
$v does BuildsHash;
is-deeply $v.h, %(a => 1), 'BUILD populates a hash attribute on a does-mixed plain value';

role BuildsArray { has @.a; submethod BUILD { @!a.push(1); @!a.push(2) } }
my $w = "x";
$w does BuildsArray;
is-deeply $w.a, [1, 2], 'BUILD pushes into an array attribute on a does-mixed plain value';

role TweaksArray { has @.a = (1, 2); submethod TWEAK { @!a.push(3) } }
my $s = "hi";
my $s2 = $s but TweaksArray;
is-deeply $s2.a, [1, 2, 3], 'TWEAK appends to an initialized array attribute on a but-mixed value';

role TweaksHash { has %.h; submethod BUILD { %!h<x> = 1 }; submethod TWEAK { %!h<y> = 2 } }
my $u = 1;
my $u2 = $u but TweaksHash;
is-deeply $u2.h, %(x => 1, y => 2), 'TWEAK adds a key to a hash attribute populated by BUILD on a but-mixed value';

# scalar attributes on the same role as an array attribute keep working
# alongside the array/hash fix (no regression on the sigil dispatch).
role Mixed { has $.x; has @.a; submethod BUILD { $!x = 7; @!a.push(9) } }
my $m = 0;
$m does Mixed;
is-deeply ($m.x, $m.a.List), (7, (9,)), 'scalar and array attributes both populate from the same BUILD';
