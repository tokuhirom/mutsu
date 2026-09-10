use v6;
use Test;

# Pins the behaviour that the #7736 intern-reduction threads a single
# pre-interned Symbol through: the fixed per-call env keys (`@_`, `self`,
# `?CLASS`, `?FILE`), the per-parameter path (value bind, type constraint,
# readonly mark) and the declaring-package switch. Each of these used to
# resolve its name through `Symbol::intern` at every helper it passed; they now
# share one symbol, so a mix-up would show up as a *wrong name* being bound,
# marked or cleared rather than as a slowdown.

plan 21;

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
