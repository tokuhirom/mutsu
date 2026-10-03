use Test;

# ADR-11276 slice 1b: a built-in method call on a variable whose method has a
# row in the built-in method table is answered by the row through a per-site
# memo, past the CallMethodMut probes. Every answer below must be the one the
# full dispatch path gives; a call is repeated so the memo is filled and hit.

plan 11;

my @a = 1, 2, 3;
my $l = (1, 2, 3, 4);
my %h = a => 1, b => 2;
my $s = "hello";
my $n = 1.5e0;
my $r = 2/6;

my @got;
for ^3 { @got.push: (@a.elems, $l.end, %h.elems, $s.chars, $n.isNaN, $r.numerator, $r.denominator).join(",") }
is @got.unique.join("|"), "3,3,2,5,False,1,3", 'row answers are stable across memo hits';

# One site, one method name, receivers of several shapes: the memo is keyed by
# the method name, so a shape change must miss and re-resolve, never reuse the
# previous shape's row.
sub elems-of($x) { $x.elems }
is (elems-of([1, 2]), elems-of({ a => 1, b => 2, c => 3 }), elems-of("abcd"),
    elems-of(1.5e0), elems-of(2/5), elems-of([1, 2])).join(","),
    "2,3,1,1,1,2", 'a site that sees several receiver shapes';

# Receivers the lane must leave to the full path.
my @lazy = 1..*;
throws-like { @lazy.elems }, X::Cannot::Lazy, 'a lazy array still refuses .elems';

class A is Array {}
my $aa = A.new(1, 2, 3, 4);
is $aa.elems, 4, 'an Array subclass instance delegates to its storage';

my $itemized = [1, 2, 3];
is $itemized.elems, 3, 'an itemized Array';

my $nan = NaN;
my @nan-seen;
for NaN, 1e0, Inf -> $v { $nan = $v; @nan-seen.push: $nan.isNaN }
is @nan-seen.join(","), "True,False,False", 'Num.isNaN on NaN, a finite Num and Inf';

my $big = 10**30 / 7;
is $big.numerator, 10**30, 'Rat.numerator with a big numerator';

# Mixins and Failures are not plain receivers.
my $mixed = "abc" but role { method chars { 99 } };
is $mixed.chars, 99, 'a mixin role method wins over the row';

sub failing() { fail "boom" }
my $f = failing();
throws-like { $f.chars }, X::AdHoc, 'an unhandled Failure explodes instead of answering .chars';

# A compile-time augment of the receiver's type wins over the row.
use MONKEY-TYPING;
augment class Array { method end { 42 } }
my @b = 1, 2, 3;
my @ends;
@ends.push: @b.end for ^2;
is @ends.join(","), "42,42", 'an augmented method wins over the row on every call';

# A row on a variable is answered in a hot loop.
my $count = 0;
$count += @a.elems for ^1000;
is $count, 3000, 'a hot loop over a row';
