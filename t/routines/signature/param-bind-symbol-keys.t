use v6;
use Test;

# Pins the behaviour that the #7736 intern-reduction threads a single
# pre-interned Symbol through: the fixed per-call env keys (`@_`, `self`,
# `?CLASS`, `?FILE`), the per-parameter path (value bind, type constraint,
# readonly mark) and the declaring-package switch. Each of these used to
# resolve its name through `Symbol::intern` at every helper it passed; they now
# share one symbol, so a mix-up would show up as a *wrong name* being bound,
# marked or cleared rather than as a slowdown.

plan 55;

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
