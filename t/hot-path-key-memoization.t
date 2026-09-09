use Test;

# Pins the observable behaviour of the closure-call / env-helper hot-path
# memoizations landed for #7571. Every one of them replaced a per-call
# `format!` + `Symbol::intern` (or a per-call set rebuild) with a memo or a
# pre-interned well-known symbol, so none of them is *supposed* to change an
# answer -- which is exactly why they need a test that would notice if one did.
#
# The four mechanisms under test:
#   * `our_var_unqualified` -- the short-circuit that lets `our_package_var_key`
#     skip its package-chain walk for a name no `our` variable carries. A wrong
#     filter makes an `our @a`/`our %h` read resolve to the wrong binding (or to
#     nothing) from inside the declaring package's routines.
#   * the memoized `__mutsu_sigilless_alias::` / `__mutsu_sigilless_readonly::`
#     / `__mutsu_state_key::` env keys, probed on every closure return.
#   * the fused single-pass `merge_sigilless_alias_writes`.
#   * the well-known `_` / `!` / `self` / `&?BLOCK` / `__mutsu_callable_id`
#     symbols the closure-call setup and its exit writeback now use instead of
#     re-interning the literals.

plan 25;

# --- `our` containers read through the package chain ------------------------
module Pkg {
    our @list = 1, 2, 3;
    our %map = (a => 10, b => 20);
    our $scalar = 100;

    sub totals() is export { @list.sum + %map<a> + $scalar }
    # A same-named LOCAL must win over the package variable, and must not be
    # redirected into it by the filter.
    sub shadowed() is export {
        my @list = 7, 8;
        my %map = (a => 9);
        @list.sum + %map<a>
    }
    # A name no `our` variable carries at all: the case the filter
    # short-circuits. It must still read the routine's own lexical.
    sub untouched() is export {
        my @fresh = 4, 5;
        @fresh.sum
    }
}
import Pkg;

is totals(), 116, 'a routine reads its package our @list / our %map / our $scalar';
is shadowed(), 24, 'a same-named my inside the package shadows the our variable';
is untouched(), 9, 'a name no our variable carries reads the plain lexical';
is @Pkg::list.join(','), '1,2,3', 'the qualified our @list is unchanged';
is %Pkg::map<b>, 20, 'the qualified our %map is unchanged';

@Pkg::list.push(4);
is totals(), 120, 'a push through the qualified name is seen by the routine';

# A package that declares NO `our` variable at all still resolves its lexicals.
module Bare {
    sub only-lexicals() is export { my @x = 1, 2; my %y = (k => 3); @x.sum + %y<k> }
}
import Bare;
is only-lexicals(), 6, 'a package with no our variables resolves its lexicals';

# Two packages owning the same bare `our` name must stay separate.
module TwinX { our @same = 'x'; }
module TwinY { our @same = 'y'; }
is @TwinX::same.join(','), 'x', 'two packages keep separate our @same (first)';
is @TwinY::same.join(','), 'y', 'two packages keep separate our @same (second)';

# --- sigilless aliases through a closure ------------------------------------
class Holder {
    has $.v is rw;
    # `my \alias = $!v` creates the sigilless-alias metadata whose env keys the
    # closure exit path probes for every free variable.
    method through-block() {
        my \alias = $!v;
        my $out;
        for 1 { $out = alias }
        $out
    }
    method attr-in-closure() {
        my &get = { $!v };
        get()
    }
}
is Holder.new(v => 41).through-block(), 41, 'a sigilless alias survives a nested block';
is Holder.new(v => 42).attr-in-closure(), 42, 'an attribute reads through a closure';

sub sigilless-rw(\x) { x = x + 1; x }
my $box = 5;
is sigilless-rw($box), 6, 'a sigilless rw parameter still writes through';

# A destructuring pointy parameter (`-> [$a, $b]`) is the shape that arms the
# sigilless-alias metadata in Digest::RIPEMD's own hot loop.
my @pairs = [1, 2], [3, 4];
is @pairs.map(-> [$a, $b] { $a * $b }).join(','), '2,12',
   'a destructuring pointy parameter still binds both slots';

# --- state variables (the memoized `__mutsu_state_key::` probe) -------------
sub counter() { state $n = 0; ++$n }
counter(); counter();
is counter(), 3, 'a state variable still accumulates across calls';

sub state-in-block() { my @seen; for ^3 { state $s = 0; @seen.push(++$s) }; @seen.join(',') }
is state-in-block(), '1,2,3', 'a state variable inside a loop body accumulates';

# --- the well-known per-call env keys ---------------------------------------
# `$_` (wk::topic) captured lexically by a block called from another routine.
sub call-it(&code) { code() }
my $topic-seen;
given 'given-topic' { my &b = { $_ }; $topic-seen = call-it(&b) }
is $topic-seen, 'given-topic', 'a block keeps its lexically captured $_';

# A routine gets its own fresh `$_`.
sub own-topic() { $_ }
given 'outer' { is own-topic(), Any, 'a routine resets its own $_ to Any' }

# `$!` (wk::error_var) is lexical to the creating scope for a block.
try { die 'boom' }
my $err-seen = call-it({ $! ?? $!.message !! 'none' });
is $err-seen, 'boom', 'a block sees the $! of its creation scope';

# `&?BLOCK` (wk::block_var) self-reference.
my $fact = -> $n { $n <= 1 ?? 1 !! $n * &?BLOCK($n - 1) };
is $fact(5), 120, '&?BLOCK still names the running block';

# `__mutsu_callable_id` (wk::callable_id) targets the right frame for `return`.
sub with-inner-return() { my &inner = sub { return 'inner' }; inner() ~ '/outer' }
is with-inner-return(), 'inner/outer', 'a return from an inner sub targets its own frame';

# `self` (wk::self_) is lexical: a block that escapes into another object's
# method must still see its creator's invocant.
class Escapee {
    has $.tag;
    method make-block() { my &b = { $.tag }; &b }
}
class Runner {
    has $.tag;
    method run(&blk) { blk() }
}
is Runner.new(tag => 'runner').run(Escapee.new(tag => 'escapee').make-block()),
   'escapee', 'a block keeps its creator self when run inside another method';

# `@_` (wk::positional_slurpy) is still bound and not written back.
sub uses-args { @_.join(',') }
is uses-args(1, 2, 3), '1,2,3', '@_ is still bound for a signature-less sub';

# --- placeholder parameters (the memoized `^name` env key) ------------------
is (1, 2, 3).map({ $^a * 2 }).join(','), '2,4,6', 'a placeholder parameter still binds';
is (1, 2).map({ $^x + $^y }).elems, 1, 'two placeholders consume two arguments';

# --- a closure calling a closure that mutates a captured outer lexical ------
# The exit-path writeback scan (whose param/local/captured sets this change
# reworked) is what propagates this back to the caller.
my $sum = 0;
my &add = -> $n { $sum += $n };
(1, 2, 3).map(-> $n { add($n) });
is $sum, 6, 'a nested closure mutation reaches the caller lexical';
