use v6;
use Test;
use nqp;

# `Promise` factory/class methods (`.start`, `.in`, `.at`, `.anyof`, `.allof`,
# `.then`) must respect the invocant when it is a user subclass of Promise
# (`my class Meows is Promise {}`), the same way `.new` already did.
#
# Three independent bugs, all regressed here:
#  - the factory methods baked in a plain "Promise" instead of threading the
#    subclass name through (`.WHAT`/`.WHAT.^name` stayed "Promise");
#  - the subclass name that WAS threaded through was the raw, ADR-0047-
#    mangled internal storage key for a `my class` declaration
#    (`Meows\u{0}<decl-id>`), not the clean user-facing "Meows" -- so
#    `.^name` rendered a stray embedded NUL + decl-id ("Meows\09", visible in
#    a terminal as "Meows" bleeding into whatever printed next), and
#    `.isa(Meows)` was False because the mangled name baked into the Promise
#    no longer matched the mangled name `Meows` (the argument) resolves to;
#  - separately, `Interpreter::dispatch_mro`'s catch-all fallback for a
#    `ValueView::Promise` used `value_type_name`, which is hardcoded to the
#    literal string "Promise" for every Promise value regardless of
#    subclass -- so `nqp::istype($meows_promise, Meows)` was False even
#    after the first two fixes made `.isa(Meows)` (a separate, Promise-aware
#    code path) True. This is exactly what the real Test.rakumod's
#    `isa-ok` calls for a non-Str expected type
#    (`nqp::istype($var, $type.WHAT)`), so it only surfaced under
#    `MUTSU_REAL_TEST=1`, not the native Test provider.
#
# See roast/S17-promise/basic.t's "subclasses create subclassed Promises"
# subtest.

plan 17;

my class Meows is Promise {};

my $started = Meows.start({ 42 });
is $started.^name, 'Meows', '.start returns an instance of the subclass (.^name)';
is $started.WHAT.^name, 'Meows', '.start .WHAT.^name is the subclass';
isa-ok $started, Meows, '.start isa-oks against the subclass';

my $chained = $started.then({ 1 });
is $chained.^name, 'Meows', '.then (while waiting) returns the subclass';

await $started;
my $chained2 = $started.then({ 1 });
is $chained2.^name, 'Meows', '.then (already completed) returns the subclass';

is Meows.in(1).^name, 'Meows', '.in returns the subclass';
is Meows.at(now).^name, 'Meows', '.at returns the subclass';
is Meows.anyof(start {}).^name, 'Meows', '.anyof returns the subclass';
is Meows.allof(start {}).^name, 'Meows', '.allof returns the subclass';
is Meows.new.^name, 'Meows', '.new (already worked) still returns the subclass';

# A plain (non-subclassed) Promise is unaffected.
my $plain = Promise.start({ 1 });
is $plain.^name, 'Promise', 'a plain Promise.start still reports "Promise"';
is $plain.WHAT.^name, 'Promise', 'a plain Promise.start.WHAT.^name is "Promise"';

is Meows.start({1}).isa(Meows), True, '.start-produced instance isa(Meows)';
is Meows.in(1).isa(Meows), True, '.in-produced instance isa(Meows)';
is Meows.start({1}).isa(Promise), True, '.start-produced instance still isa(Promise)';

# `nqp::istype` (what the real Test.rakumod's isa-ok actually calls for a
# non-Str expected type) must agree with `.isa` -- see dispatch_mro above.
is so(nqp::istype(Meows.start({1}), Meows)), True,
    'nqp::istype(subclass-Promise, Meows) is True';
is so(nqp::istype(Meows.start({1}), Meows.WHAT)), True,
    'nqp::istype(subclass-Promise, Meows.WHAT) is True';

done-testing;
