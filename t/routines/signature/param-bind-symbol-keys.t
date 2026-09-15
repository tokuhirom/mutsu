use v6;
use Test;

# Pins the behaviour that the #7736 intern-reduction threads a single
# pre-interned Symbol through: the fixed per-call env keys (`@_`, `self`,
# `?CLASS`, `?FILE`), the per-parameter path (value bind, type constraint,
# readonly mark) and the declaring-package switch. Each of these used to
# resolve its name through `Symbol::intern` at every helper it passed; they now
# share one symbol, so a mix-up would show up as a *wrong name* being bound,
# marked or cleared rather than as a slowdown.
#
# The last section extends that to #7766 unit 2, which threads the *callsite*
# name and package as symbols into `call_compiled_function_named` and the
# resolution probe below it. Those two names are what the routine frame,
# `&?ROUTINE`, the callable-id key, the `LEAVE` routine key and the
# declaring-package switch are all built from, so the same class of mistake --
# the wrong name reaching one of them -- is what these assertions catch.

plan 96;

# --- the fixed per-call keys -------------------------------------------------

sub legacy-args { @_.join(",") }
is legacy-args(1, 2, 3), "1,2,3", '@_ still carries the positional arguments';
is legacy-args(), "", '@_ is empty for a no-argument call';

sub nested-args { legacy-args(7, 8) ~ "|" ~ @_.join(",") }
is nested-args(9), "7,8|9", 'a nested call restores the caller @_';

class Invocant {
    has $.tag = "t";
    method who() { self.tag ~ "/" ~ ::?CLASS.^name }
}
is Invocant.new.who, "t/Invocant", 'self and ::?CLASS resolve inside a method';

ok $?FILE.ends-with("param-bind-symbol-keys.t"), '$?FILE names this file';

# --- per-parameter binding ---------------------------------------------------

sub two-params($cond, $desc) { "$cond:$desc" }
is two-params(1, "x"), "1:x", 'two positional parameters bind to their own names';

sub with-default($a, $b = "d") { "$a$b" }
is with-default("q"), "qd", 'an omitted optional parameter binds its default';
is with-default("q", "r"), "qr", 'a supplied optional parameter wins over the default';

sub named-param(:$unescaped-prefix = "n") { $unescaped-prefix }
is named-param(), "n", 'a named parameter falls back to its default';
is named-param(:unescaped-prefix<y>), "y", 'a named parameter binds by name';

# --- readonly marking --------------------------------------------------------

sub readonly-param($x) { $x = 5 }
dies-ok { readonly-param(1) }, 'a plain scalar parameter is readonly';

sub copy-param($x is copy) { $x = 5; $x }
is copy-param(1), 5, 'an `is copy` parameter is writable';

my $outer = 1;
sub shadow($outer is copy) { $outer = 2; $outer }
is shadow(9), 2, 'a writable parameter is not held readonly by an outer name';
lives-ok { $outer = 3 }, 'the caller lexical stays writable after the call';

# --- placeholder parameters (the `^`-twigil env key) -------------------------

my &placeholder = { $^first ~ $^second };
is placeholder("a", "b"), "ab", 'placeholder parameters bind in order';

# --- sigilless / raw parameters ----------------------------------------------

sub sigilless(\v) { v * 2 }
is sigilless(21), 42, 'a sigilless parameter binds the raw value';

sub raw-alias($x is rw) { $x = $x + 1 }
my $rw = 1;
raw-alias($rw);
is $rw, 2, 'an `is rw` parameter writes back through the caller container';

# --- typed parameters and constraint scoping ---------------------------------

sub typed(Int $n) { $n + 1 }
is typed(1), 2, 'a typed parameter accepts a matching argument';
my $bad = "s";
dies-ok { typed($bad) }, 'a typed parameter rejects a mismatching argument';

sub untyped-inner($v) { $v }
my Str $v = "s";
is untyped-inner(1), 1, "an untyped parameter shadows the caller's typed lexical";
lives-ok { $v = "t" }, "the caller's own typed lexical keeps its constraint";

# --- #7766: the callee's BAKED parameter symbols ------------------------------
# A compiled routine now hands the binder its registration-time
# `param_name_syms` instead of interning each parameter name per call, so the
# slice is indexed by parameter position. A misalignment would bind, mark or
# clear a parameter under a *neighbour's* name, which these pin by giving every
# position a distinguishable value.

sub positional-order($a, $b, $c) { "$a|$b|$c" }
is positional-order(1, 2, 3), "1|2|3", 'three positionals bind in signature order';

sub mixed-shape($head, @rest, :$opt = "o", *%extra) {
    "$head/{@rest.join(',')}/$opt/{%extra.keys.sort.join(',')}"
}
is mixed-shape("h", [1, 2], :opt<p>, :x, :y), "h/1,2/p/x,y",
    'a mixed positional/array/named/slurpy signature binds each position to its own name';

sub after-subsig([$p, $q], $tail) { "$p$q$tail" }
is after-subsig([1, 2], 3), "123",
    'a parameter following a destructuring sub-signature still binds its own name';

sub invocant-then-params($n, $m) { "$n-$m" }
is invocant-then-params(4, 5), "4-5", 'positions stay aligned across repeated calls';
is invocant-then-params(6, 7), "6-7", 'a second call rebinds the same positions';

class Baked {
    method two($x, $y is copy) { $y = $y + 1; "$x:$y" }
}
is Baked.new.two(1, 2), "1:3",
    'a method marks only the `is copy` parameter writable, by position';

# --- #7766: the `__mutsu_scalar_bind_no_container::` marker -------------------
# `my $i := 42` records "this name owns no Scalar container" under a per-local
# pre-interned key, and every other scalar declaration clears it so a
# same-named redeclaration cannot inherit it. A key mix-up shows up as `.self
# =:= $name` answering for the wrong variable.

my $bound := 42;
ok $bound.self =:= $bound, 'a `:=`-to-value scalar owns no Scalar container';
my $assigned = 42;
nok $assigned.self =:= $assigned, 'an assigned scalar does own a Scalar container';

sub container-id($bind) {
    if $bind { my $v := 9; return $v.self =:= $v }
    else     { my $v = 9;  return $v.self =:= $v }
}
ok container-id(True), 'the marker is set for the bound declaration';
nok container-id(False), 'a later same-named assigned declaration clears it';
ok container-id(True), 'and the marker is set again on the next bound declaration';

# --- #7766 unit 2: the read-modify-write ops' name symbol ---------------------
# `++`, `--` and the fused `$x OP= rhs` take their target's name from a
# constant-pool operand, and their tail probes it against four Symbol-keyed
# stores: the readonly registry, the env, the declared-type lane and the
# native-int constraint. Those probes now share the one symbol
# `CompiledCode::const_sym` memoizes per chunk instead of re-interning the
# string at each layer. A mix-up would read or write the WRONG NAME's entry, so
# every case below gives its neighbours distinguishable values.

# the native-int constraint lane (wrapping) vs. a boxed neighbour
my int8 $i8 = 127;
$i8++;
is $i8, -128, 'int8 ++ wraps at the top';
my int8 $j8 = -128;
$j8--;
is $j8, 127, 'int8 -- wraps at the bottom';
my int8 $k8 = 120;
$k8 += 10;
is $k8, -126, 'int8 += wraps';
my Int $boxed = 127;
$boxed++;
is $boxed, 128, 'a boxed Int neighbour does not wrap';

# the declared-type lane, re-checked after the mutation
subset Even of Int where * %% 2;
my Even $even = 2;
dies-ok { $even++ }, 'a subset constraint rejects the incremented value';
is $even, 2, 'the rejected increment leaves the variable untouched';
lives-ok { $even += 2 }, 'a compound assign satisfying the constraint is accepted';
is $even, 4, 'and stores the new value';

# the readonly registry, and the separate sigilless-readonly marker
sub ro-param($n) { $n++ }
dies-ok { ro-param(1) }, 'a readonly parameter rejects ++';
sub rw-copy($n is copy) { $n++; $n }
is rw-copy(1), 2, 'an `is copy` parameter accepts ++';
my \Gbound = 5;
dies-ok { Gbound++ }, 'a sigilless bind to a value rejects ++';

# two same-named locals in sibling scopes: one symbol, two slots
sub branch-local($flag) {
    if $flag { my $c = 10; $c++; return $c }
    else     { my $c = 20; $c += 5; return $c }
}
is branch-local(True), 11, 'the then-branch local increments its own value';
is branch-local(False), 25, 'the else-branch local compounds its own value';

# the env store: an `our` scalar writes through its canonical cell
our $pkg-counter = 1;
$pkg-counter++;
is $pkg-counter, 2, 'an `our` scalar increments through its canonical cell';
$pkg-counter ~= "!";
is $pkg-counter, "2!", 'an `our` scalar concatenates through the same cell';

# a dynamic variable incremented from a callee
my $*dyn-counter = 1;
sub bump-dyn() { $*dyn-counter++ }
bump-dyn();
is $*dyn-counter, 2, 'a dynamic variable increments in the declaring frame';

# the topic, whose store has its own writeback arm
my @src = 1, 2, 3;
my @mapped = @src.map({ $_ += 1 });
is @mapped.join(","), "2,3,4", 'a compound assign to the topic yields the new value';
is @src.join(","), "2,3,4", 'and writes back through the rw topic alias';
for @src <-> $x { $x++ }
is @src.join(","), "3,4,5", 'a rw loop alias writes back';

# distinct names must not alias one another
my $first = 1;
my $second = 100;
$first++;
$second += 2;
is $first, 2, 'the first name increments alone';
is $second, 102, 'the second name compounds alone';

my $chained = "a";
$chained ~= "b";
$chained x= 2;
is $chained, "abab", 'string compound assignment chains on one name';

my $repeated = 0;
$repeated++ for ^5;
is $repeated, 5, 'repeated increments accumulate on one name';

# --- #7766: the callsite name and package carried as symbols -----------------

# `&?ROUTINE` is built from the two symbols the named entry is handed.
sub routine-identity($a) { &?ROUTINE.name ~ "/" ~ &?ROUTINE.package.^name }
is routine-identity(1), "routine-identity/GLOBAL",
    'a by-name call names its own routine and package';
is routine-identity(2), "routine-identity/GLOBAL",
    'and answers the same on a second call, not a neighbour name';

sub other-identity($a) { &?ROUTINE.name }
is other-identity(1), "other-identity",
    'a second routine of the same shape keeps its own name';
is routine-identity(3), "routine-identity/GLOBAL",
    'the first routine is unaffected by the second having been called';

# The declaring package, not the callsite's, is what the body resolves against.
package Deep {
    our $inside = "deep-var";
    our sub reach() { $inside }
    our sub reach-nested() { reach() ~ "+nested" }
}
is Deep::reach(), "deep-var",
    'a cross-package call resolves a package variable of its DECLARING package';
is Deep::reach(), "deep-var", 'and does so again on a repeat call';
is Deep::reach-nested(), "deep-var+nested",
    'an unqualified call inside that body resolves in the same package';
is $Deep::inside, "deep-var", 'the package variable is untouched from outside';

# #8347: a qualified callsite (`Pkg::sub()`) must not leak its `Pkg::`
# qualification into the callee's own frame name -- `&?ROUTINE.name` reads
# the routine's own short name regardless of how the caller spelled it.
package Deep {
    our sub qualified-name-probe() {
        &?ROUTINE.name ~ "/" ~ &?ROUTINE.package.^name;
    }
}
is Deep::qualified-name-probe(), "qualified-name-probe/Deep",
    'a qualified callsite names the routine by its own short name, not the callsite text';
is Deep::qualified-name-probe(), "qualified-name-probe/Deep",
    'and does so again on a repeat call, once the light-call cache is warm';

# An anonymous routine has an empty name and must take the `<anon>` sentinel
# rather than binding the empty string as a routine name.
my $anon = sub ($a) { $a * 2 };
is $anon(21), 42, 'an anonymous sub called through a scalar still runs';
my &code-var = &routine-identity;
is code-var(4), "routine-identity/GLOBAL",
    'a call through a `&`-sigil code variable names the routine it holds';

# `where` makes the call ineligible for the light entries, so it takes the
# named entry this change rewired.
sub wherey($a where * > 0) { &?ROUTINE.name ~ ":" ~ $a }
is wherey(1), "wherey:1", 'a where-constrained call binds and names correctly';
is wherey(2), "wherey:2", 'and again with a different argument';

# A multi candidate reports the proto's name, and each candidate keeps its own
# body -- the resolution probe is keyed on the callsite name symbol.
proto pd($) {*}
multi pd(Int $n) { "int:" ~ &?ROUTINE.name }
multi pd(Str $s) { "str:" ~ &?ROUTINE.name }
is pd(1), "int:pd", 'an Int multi candidate runs and reports the proto name';
is pd("x"), "str:pd", 'a Str multi candidate runs and reports the proto name';
is pd(2), "int:pd", 'the Int candidate is still reachable after the Str one';

# `nextsame` re-enters the dispatch chain with the NEXT candidate's own
# package/name pair.
multi chain(Int $n) { "int-" ~ nextsame }
multi chain(Any $n) { "any" }
is chain(1), "any", 'nextsame defers to the next candidate and returns ITS value';
is chain("s"), "any", 'a non-Int argument reaches the Any candidate directly';

# The `LEAVE` phaser's routine key is `"{package}::{name}"`, built from the
# same two symbols.
my $left = "";
sub leaver($a) { LEAVE { $left ~= "L" }; return $a * 3 }
is leaver(2), 6, 'a routine with a LEAVE phaser returns its own value';
is $left, "L", 'and the phaser fired exactly once';
is leaver(3), 9, 'a second call returns its own value';
is $left, "LL", 'and fires the phaser again';

# `$!` is reset per routine; the reset is gated on the name being non-empty.
sub thrower() { die "boom" }
sub catcher() { try thrower(); return $!.defined ?? "caught" !! "clean" }
is catcher(), "caught", 'a by-name call sees its own $! after a failed call';
sub quiet() { return $!.defined ?? "stale" !! "fresh" }
is quiet(), "fresh", 'a later routine starts with a fresh $!';

# --- #7766 unit 2 item 4: the resolution layers below the named entry --------
#
# `fn_keys_for_base`, `resolve_function_multi_cached`,
# `resolve_all_multi_candidates_cached` and `push_multi_dispatch_frame` now
# take the callsite name's Symbol from the caller instead of re-interning it.
# `fn_keys_for_base_sym` may only reuse that symbol when the name's BASE is the
# whole name, because the key index is keyed by the base -- so a qualified or
# arity-suffixed callee must still reach the same key set. A mix-up here shows
# up as the WRONG routine's declaration answering a question about the callee:
# its rw-ness, its candidate list, or its winner.

# ADR-0067's rw-argument container gate (`MarkRwArgRefContext` ->
# `named_routine_binds_container_at`) is the `fn_keys_for_base` consumer on the
# per-op path. It must answer for the unqualified and the qualified spelling
# alike, and must not borrow a same-named routine's answer from another package.
class RwHolder { has $.slot is rw = 1 }
sub bump($x is rw) { $x = $x + 10; $x }
package Q {
    our sub bump-q($x is rw) { $x = $x + 100; $x }
}
package R {
    our sub bump($x) { $x + 1000 }
}
my $rw-h = RwHolder.new;
is bump($rw-h.slot), 11, 'an unqualified is-rw callee binds the caller container';
is $rw-h.slot, 11, 'and the write reached the attribute through it';
my $rw-h2 = RwHolder.new;
is Q::bump-q($rw-h2.slot), 101,
    'a package-qualified is-rw callee binds the caller container too';
is $rw-h2.slot, 101, 'and its write reached the attribute as well';
is R::bump(5), 1005,
    'a same-named sub in another package keeps its own non-rw signature';

# Two protos of the same short name in different packages: the multi
# resolution cache and the candidate memo are keyed on (package, name symbol),
# so a wrong name symbol would serve one package's winner for the other.
package M1 {
    our proto mm($) {*}
    our multi mm(Int $n) { "M1-int" }
    our multi mm(Str $s) { "M1-str" }
}
package M2 {
    our proto mm($) {*}
    our multi mm(Int $n) { "M2-int" }
    our multi mm(Str $s) { "M2-str" }
}
is M1::mm(1), "M1-int", 'a qualified multi call picks its own package Int candidate';
is M2::mm(1), "M2-int", 'and the same short name in another package picks that one';
is M1::mm("x"), "M1-str", 'the Str candidate of the first package is still reachable';
is M2::mm("y"), "M2-str", 'and of the second';
is M1::mm(2), "M1-int", 'the first package resolves again once both caches are warm';

# A three-deep `nextsame` chain exercises the dispatch frame the
# `push_multi_dispatch_frame_sym` path builds: the frame's `remaining` list
# comes from the name-keyed candidate memo, so a wrong key would shorten or
# misorder it.
multi deep(Int $n) { "i" ~ nextsame }
multi deep(Cool $n) { "c" ~ nextsame }
multi deep(Any $n) { "a" }
is deep(1), "a", 'a three-deep nextsame chain reaches the last candidate';
is deep(1), "a", 'and does so again with the candidate memo warm';

# `native_lever_a_user_override` now takes the method symbol from its caller.
# A user method augmented onto a builtin must still win over the native row of
# the same name, and an un-overridden native method must still be served.
use MONKEY-TYPING;
augment class Str { method shout() { self.uc ~ "!" } }
is "hi".shout, "HI!", 'a user method augmented onto a builtin is dispatched';
is "hi".uc, "HI", 'and an un-overridden native method still serves';

# The parameter type-constraint and readonly marks travel through the two
# `debug_assert`s this change stopped from interning; both must still bite.
sub typed-again(Int $n) { $n * 2 }
is typed-again(21), 42, 'a typed parameter still binds and runs';
dies-ok { my $ro = sub ($a) { $a = 5 }; $ro(1) },
    'a readonly parameter still refuses assignment';
