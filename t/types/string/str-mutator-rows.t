use Test;

# `Str.subst-mutate` and `Str.substr-rw` are rows of the one method table
# (ADR-11276 §9.23): `Handler::Mut` rows that need the receiver's binding, since
# a string is immutable and the variable is what changes.

plan 16;

my $s = "hello world";
my $m = $s.subst-mutate("o", "0");
is $s, 'hell0 world', 'subst-mutate replaces in the variable';
is $m.WHAT.gist, '(Match)', 'and answers the Match for a single hit';
is $m.from, 4, 'which carries its position';

my $t = "aaa";
my $all = $t.subst-mutate("a", "b", :g);
is $t, 'bbb', ':g replaces every hit';
is $all.elems, 3, 'and answers a Match for each';

my $u = "aaa";
$u.subst-mutate("a", "X", :nth(2));
is $u, 'aXa', 'an adverb of the pattern (:nth) is bound by the row';

my $v = "abcabc";
$v.subst-mutate("b", "X", :x(1));
is $v, 'aXcabc', ':x(1) stops after one replacement';

my $none = "abc";
$none.subst-mutate("zzz", "y");
is $none, 'abc', 'a miss leaves the variable alone';

# the variable is what changes: every way a name can hold the string
sub copy-and-mutate($x is copy) { $x.subst-mutate("a", "b"); $x }
is copy-and-mutate("banana"), 'bbnana', 'a parameter that is a copy';

for "x1", "x2" -> $e is copy {
    $e.subst-mutate("x", "y");
    is $e, $e.starts-with('y') ?? $e !! 'unreachable', 'a loop variable that is a copy';
}

our $pkg = "qqq";
$pkg.subst-mutate("q", "r");
is $pkg, 'rqq', 'a package variable';

# substr-rw: the Proxy writes through to the variable it came from
my $w = "abcdef";
$w.substr-rw(1, 2) = "XY";
is $w, 'aXYdef', 'assigning to the window splices into the variable';
{
    my $r := $w.substr-rw(0, 1);
    is $r, 'a', 'a bound window reads the current text';
    $r = "ZZ";
    is $w, 'ZZXYdef', 'and writes through when assigned';
    $r = "Q";
    is $w, 'QXYdef', 'the window spans the new text after a store';
}

done-testing;
