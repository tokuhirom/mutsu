use Test;

# Each env tier memoizes the subset of its keys a closure capture could ever
# keep, so a wide scope's never-captured metadata (routine-registration markers,
# attribute-twigil materializations) is skipped without being visited
# (`src/env_tier.rs`, #7565). The memo is keyed to the tier's KEY SET: a value
# write must not disturb it and a new key must rebuild it.
#
# The padding below is what makes this file test the memoized path at all — the
# memo is only built for a tier wide enough to pay for it, so a scope with a
# handful of names would exercise the plain walk instead and prove nothing.

plan 14;

my $Pad01 = 1; my $Pad02 = 2; my $Pad03 = 3; my $Pad04 = 4; my $Pad05 = 5;
my $Pad06 = 6; my $Pad07 = 7; my $Pad08 = 8; my $Pad09 = 9; my $Pad10 = 10;
my $Pad11 = 11; my $Pad12 = 12; my $Pad13 = 13; my $Pad14 = 14; my $Pad15 = 15;
my $Pad16 = 16; my $Pad17 = 17; my $Pad18 = 18; my $Pad19 = 19; my $Pad20 = 20;
sub pad-a() { 'a' }; sub pad-b() { 'b' }; sub pad-c() { 'c' }; sub pad-d() { 'd' }
sub pad-e() { 'e' }; sub pad-f() { 'f' }; sub pad-g() { 'g' }; sub pad-h() { 'h' }
sub pad-i() { 'i' }; sub pad-j() { 'j' }; sub pad-k() { 'k' }; sub pad-l() { 'l' }

# --- The wide scope's names are still all visible from a capture -------------
is { $Pad07 }(), 7, 'an uppercase-initial lexical is captured from a wide scope';
is { Int }().gist, '(Int)', 'a type name resolves inside a closure in a wide scope';
is { pad-c() }(), 'c', 'an enclosing named sub is callable from the closure';

my $*WIDE = 'dyn';
is { $*WIDE }(), 'dyn', 'a dynamic is captured from a wide scope';

# --- Repeated creation from the same wide scope ------------------------------
# The memo lives on the tier, so every one of these creations reads the same
# candidate list. Each closure must still capture its own free variable.
sub make($n) { return { $n * 3 } }
is (1 .. 5).map({ make($_).() }).join(','), '3,6,9,12,15',
    'each closure created from the wide scope keeps its own free variable';

# --- A VALUE write must not disturb the memo ---------------------------------
$Pad07 = 70;
is { $Pad07 }(), 70, 'a value write to a wide-scope name is seen by a later capture';
is make(2).(), 6, 'and the repeated-creation path is unaffected by it';

# --- A NEW key must rebuild it ------------------------------------------------
# `$Late` is declared after the memo above was built. A capture that missed it
# would leave the closure unable to see the name at all.
my $Late = 'late';
is { $Late }(), 'late', 'a name declared after the memo was built is still captured';
sub late-sub() { 'late-sub' }
is { late-sub() }(), 'late-sub', 'so is a routine declared after it';

# --- Shadow metadata still follows its subject --------------------------------
my Int $Typed = 3;
is { $Typed }(), 3, 'a typed wide-scope lexical is captured';
dies-ok { my $w = { $Typed = 'str' }; $w() },
    'and its type constraint is captured with it';

my Int $*TypedDyn = 4;
is { $*TypedDyn }(), 4, 'a typed dynamic is captured';
dies-ok { my $w = { $*TypedDyn = 'str' }; $w() },
    'and so is its constraint (it is a system name, never a free variable)';

# --- `self` reaches through a capture made in a wide method scope --------------
class Wide {
    has $.v;
    method wrapped() {
        my $q01 = 1; my $q02 = 2; my $q03 = 3; my $q04 = 4; my $q05 = 5;
        my $q06 = 6; my $q07 = 7; my $q08 = 8; my $q09 = 9; my $q10 = 10;
        return { self.v + $q07 }
    }
}
is Wide.new(v => 5).wrapped().(), 12, 'self and a method local both reach the closure';
