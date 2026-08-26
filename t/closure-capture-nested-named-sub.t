use Test;

# A block/sub-literal's closure capture set must include a free variable
# referenced ONLY from inside a nested named `sub`'s body -- the compiler's
# free-variable scan used to fold in a nested ANONYMOUS closure's free vars
# (`closure_compiled_codes`) but never a nested named `sub`'s, since a named
# sub is registered via `RegisterDecl` rather than embedded as a closure.
# See todo/tickets/block-capture-misses-free-vars-used-only-by-inner-named-sub.md
# (now news/2026-08/).
#
# Each case below is verified against `raku` (v2026.06) as well as mutsu.
#
# IMPORTANT, twice over:
#
# 1) Every case uses variable/sub names unique to that case (never reusing
#    e.g. `$l`/`&blk`/`nested` across cases) -- a bare `{ ... }` test-scope
#    block is compiled INLINE into the surrounding mainline code, so a name
#    reused across sibling blocks can share the same top-level local slot.
#
# 2) Cases are NOT wrapped in a bare `{ ... }` scope block at all (unlike most
#    other `t/*.t` files) -- every case is flat top-level code. This is
#    load-bearing, not stylistic: a plain top-level `{ ... }` statement
#    compiles via `OpCode::BlockScope`, whose conservative env-sync gate
#    (`mark_name_access_slots`/`env_consumer_slots.block_scope` in
#    `compute_needs_env_sync`) unconditionally syncs every local the block's
#    body touches into the name-keyed env -- for EVERY SUCH BLOCK EXCEPT THE
#    LAST ONE in a compilation unit (the last bare block compiles through the
#    lighter tail-position `compile_bare_block_inline` path instead, which
#    does not). That blanket sync happens to make a nested named sub's
#    outer-var read resolve correctly even WITHOUT this file's fix, for every
#    case except the textually-last one -- so wrapping each case in `{ ... }`
#    would silently turn 14 of these 15 assertions into no-ops that pass on
#    the unfixed compiler. Confirmed by reverting the fix locally and
#    re-running this file both ways while writing it. Keep every case flat.

plan 15;

# 1) The core repro: a variable read ONLY inside a nested named sub, with the
#    block invoked as a stored Callable value (not run inline).
my $cap1 = 42;
my &blk1 = { sub nested1() { $cap1 }; nested1() };
is blk1(), 42, 'block value captures a var read only by a nested named sub';

# 2) `our` variable read by a nested named sub (package storage, not a
#    lexical capture -- must keep working, unaffected by the fix).
our $cap2 = 99;
my &blk2 = { sub nested2() { $cap2 }; nested2() };
is blk2(), 99, 'nested named sub reads an our-variable via package storage';

# 3) Sigilless alias (`my \x = ...`) read only by a nested named sub.
my $cap3 = 42;
my \alias3 = $cap3;
my &blk3 = { sub nested3() { alias3 }; nested3() };
is blk3(), 42, 'nested named sub reads a sigilless alias captured by the block';

# 4) NEGATIVE: the nested sub's own `my` declaration of the same name must
#    shadow the outer lexical -- the outer must NOT be captured on its behalf.
my $cap4 = 42;
my &blk4 = { sub nested4() { my $cap4 = 7; $cap4 }; nested4() };
is blk4(), 7, 'nested named sub my-declaration shadows the outer lexical';

# 5) NEGATIVE: the nested sub's own PARAMETER of the same name must shadow
#    the outer lexical the same way.
my $cap5 = 42;
my &blk5 = { sub nested5($cap5) { $cap5 }; nested5(100) };
is blk5(), 100, 'nested named sub parameter shadows the outer lexical';

# 6) Two levels of named-sub nesting: `sub outer { sub inner { $x } }` -- the
#    free-variable fold must be transitive through both levels.
my $cap6 = 42;
my &blk6 = { sub outer6() { sub inner6() { $cap6 }; inner6() }; outer6() };
is blk6(), 42, 'two levels of nested named subs both surface the outer var';

# 7) `multi sub`: each candidate signature is compiled separately and must
#    independently contribute its free vars.
my $cap7 = 42;
my &blk7 = {
    multi sub nested7(Int $x) { "int:$cap7:$x" }
    multi sub nested7(Str $x) { "str:$cap7:$x" }
    nested7(5) ~ "/" ~ nested7("a");
}
is blk7(), 'int:42:5/str:42:a', 'each multi candidate contributes its own free vars';

# 8) A named sub nested inside a `sub {...}` LITERAL (not a `{...}` block)
#    must be covered the same way.
my $cap8 = 42;
my &blk8 = sub { sub nested8() { $cap8 }; nested8() };
is blk8(), 42, 'named sub nested in a sub{} literal captures the outer var';

# 9) Mutation, not just reads: the write must go through a shared cell so the
#    outer scalar observes the accumulation (this already worked before the
#    fix -- pinned here so it can't silently regress alongside the read fix).
my $cap9 = 0;
my &blk9 = { sub bump9() { $cap9++ }; bump9() };
blk9();
is $cap9, 1, 'nested named sub mutation of an outer var is visible after the call';

# 10) Own-local read (the sub reads a var declared inside the SAME block,
#     not an ancestor) -- a different, pre-existing mechanism
#     (`compute_needs_env_sync`'s lazy-body env-sync gate); pinned so the new
#     fold cannot regress it.
my &blk10 = { my $cap10 = 55; sub nested10() { $cap10 }; nested10() };
is blk10(), 55, 'nested named sub still reads an own-block local correctly';

# 11) Container mutation (push) of an outer array by a nested named sub.
my @arr11 = (1, 2, 3);
my &blk11 = { sub nested11() { @arr11.push(4) }; nested11() };
blk11();
is @arr11.join(','), '1,2,3,4', 'nested named sub push-mutates an outer array';

# 12) The `Thread`/`clone_for_thread` path: the same free-var set feeds the
#     shared-variable seeding for a spawned thread's env.
my $cap12 = 42;
my $t12 = Thread.start({ sub nested12() { $cap12 } ; nested12() });
$t12.finish;
is $cap12, 42, 'Thread body with a nested named sub does not corrupt the outer var';

my $cap13 = 42;
my $out13;
my $t13 = Thread.start({
    sub nested13() { $cap13++ }
    nested13();
    $out13 = $cap13;
});
$t13.finish;
is $out13, 43, 'Thread body: nested named sub mutation is visible inside the thread';
is $cap13, 43, 'Thread body: nested named sub mutation is visible to the spawning thread';

# 13) Lock::Async.protect-or-queue-on-recursion, the shape that surfaced this
#     bug (news/2026-08/lock-async-recursion-methods-missing.md): a nested
#     named sub re-enters the SAME lock via a captured outer variable.
my $lock14 = Lock::Async.new;
my $out14;
$lock14.protect-or-queue-on-recursion({
    sub nested14() { $lock14.protect-or-queue-on-recursion({ $out14 = "in" }) }
    nested14();
});
is $out14, "in", 'Lock::Async.protect-or-queue-on-recursion reads the lock through a nested named sub';
