use v6;
use Test;

# An lvalue routine's assign target now comes from plan metadata
# (CompiledRoutineMetadata::rw_tail_expr), not from re-extracting the tail
# of the AST body — such routines register body-less like every other
# safe-class def (ADR-0019 C6e-3c lvalue keep-class). Expected values
# verified against raku.

plan 4;

my $var = 1;
sub f() is rw { $var }
f() = 5;
is $var, 5, "is rw routine assigns through its tail variable";

my $w = 1;
sub g() is rw { $w }
my &c = &g;
c() = 7;
is $w, 7, "is rw routine called as a code object assigns too";

my $m = 1;
sub h() { $m.return-rw }
h() = 9;
is $m, 9, "an explicit .return-rw tail is assignable without the trait";

sub plain() { 42 }
my $err = False;
try { plain() = 1; } // ($err = True);
ok $!.defined || $err, "assigning a non-rw routine still dies";

# NOTE: an ELEMENT tail (`sub elem() is rw { @a[1] }; elem() = 99`) is a
# pre-existing gap (X::Assignment::RO on v0.20.0 too) — see
# todo/tickets/lvalue-sub-element-tail-not-assignable.md.

done-testing;
