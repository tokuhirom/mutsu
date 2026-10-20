use Test;

# The receiver step (ADR-11276 §2.4, #14080): every dispatch entry -- a
# static method call, a call by a run-time name (`."$n"()`), and the 0- and
# 1-argument native forms -- reads a container, a hash-like receiver, a Match,
# an Array-valued enum member and a user IO::Path subclass the same way.

plan 24;

my $h = {a => 1, b => 2};
my %h = a => 1, b => 2;
my $l = (3, 1, 2);
my $a = [3, 1, 2];

for <elems reverse sort head tail List> -> $n {
    is-deeply $h."$n"(), $h."$n"(), "\$h.$n by name";
}
is $h.elems, 2, 'an itemized Hash is its pairs';
is $h.sort.map(*.key).join, 'ab', 'an itemized Hash sorts its pairs';
is $h.head(1).elems, 1, 'head(1) of an itemized Hash';
is %h.reverse.elems, 2, 'a Hash reverses its pairs';
is $l.tail, 2, 'an itemized List reads its elements';
is $l."tail"(), 2, 'an itemized List by name';
is $a.VAR.^name, 'Scalar', '.VAR of an itemized Array';
is $h.VAR.^name, 'Scalar', '.VAR of an itemized Hash';
ok $h.raku.starts-with('${'), '.raku sees the itemized Hash';
is $l.raku, '$(3, 1, 2)', '.raku sees the itemized List';
is $a."raku"(), '$[3, 1, 2]', '.raku by name sees the itemized Array';

my $m = 'abc' ~~ /(a)(b)(c)/;
is $m.elems, 3, 'a Match counts its positional captures';
is $m.List.elems, 3, 'Match.List is its positional captures';
is $m."reverse"().map(~*).join, 'cba', 'Match.reverse by name';

enum E (:x[1, 2, 3]);
is x.elems, 3, 'an Array-valued enum member answers its base';
is x."head"(2), (1, 2), '... by name too';

class P is IO::Path { }
my $p = P.new('/tmp/foo.txt');
is $p.uc, '/TMP/FOO.TXT', 'a Cool method of an IO::Path subclass reads its path';
is $p."chars"(), 12, '... by name too';
