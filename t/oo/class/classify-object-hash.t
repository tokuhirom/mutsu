use Test;

plan 9;

# §3.3 object-hash / junction key: `classify` (and `categorize`) keyed by a
# non-Str classifier result (e.g. a Junction from `*.contains: any ...`) builds
# an OBJECT hash. Its keys stay real objects (not stringified), `$h{ $key }` is a
# by-key lookup (NOT junction autothreading), and the bare-sub form parses as a
# list operator (`classify *.meth, @list`).

my @l := <abc axz abcdef axyf cbd xyz>;

# Method form.
my $m := @l.classify: *.contains: any 'a', 'f';
is $m.keys.map(*.^name).unique.join, 'Junction', 'method: object keys stay Junction';
is-deeply $m{ any(False, False) }.sort, <cbd xyz>,     'method: non-matching key lookup';
is-deeply $m{ any(True,  False) }.sort, <abc axz>,     'method: part-matching key lookup';
is-deeply $m{ any(True,  True)  }.sort, <abcdef axyf>, 'method: full-matching key lookup';

# Bare-sub (list-op) form — `classify *.meth, @list` must parse, not be read as
# `classify * (.meth ...)` (infix multiply).
my $s := classify *.contains( any 'a', 'f' ), @l;
is $s.keys.map(*.^name).unique.join, 'Junction', 'sub: object keys stay Junction';
is-deeply $s{ any(True, False) }.sort, <abc axz>, 'sub: by-key lookup';

# A plain (Str-keyed) classify is unchanged: Str keys, and a junction subscript on
# a PLAIN hash still autothreads.
my $c := <apple banana avocado>.classify(*.substr(0, 1));
is $c.keys.map(*.^name).unique.join, 'Str', 'plain classify keeps Str keys';
my %plain = a => 1, b => 2;
is-deeply %plain{ any('a', 'b') }, any(1, 2), 'plain hash junction subscript still autothreads';

# categorize bare-sub form parses too.
my $cat := categorize *.contains( any 'a', 'f' ), @l;
ok $cat.keys.elems >= 2, 'categorize sub form parses and runs';
