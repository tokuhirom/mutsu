use Test;

plan 5;

# `s`, `m` and `y` followed by a closing bracket are ordinary terms (a call
# argument named like a quote construct), not quote openers (#17176).
sub k($x) { $x }
my &s = -> { 'sub-s' };
my &m = -> { 'sub-m' };
my &y = -> { 'sub-y' };

is k(s), 'sub-s', 's before ) is a term';
is k(m), 'sub-m', 'm before ) is a term';
is k(y), 'sub-y', 'y before ) is a term';
is ([k(s)]).elems, 1, 's inside brackets still reads as a term';

class L {
    has $.vow;
    method m(&cb) { k(cb) if False; cb(True, 'late') }
}
is L.new.m(-> $ok, $r { $r }), 'late', 'a statement after k(s) is not swallowed into s)...)';
