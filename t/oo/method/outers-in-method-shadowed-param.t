use Test;

plan 8;

# `OUTERS::` inside a method body sees the declaring lexical chain even when
# the method's own parameter shadows the name (#17003).
my $n = 5;
class A {
    method t($n) { $OUTERS::n }
    method w($n) { $OUTERS::n = 99; $n }
    method inner($n) { my $m = 1; $OUTERS::n }
}
is A.new.t(1), 5, 'read past a shadowing parameter';
is A.new.inner(2), 5, 'read past a shadowing parameter with a local';
is A.new.w(1), 1, 'the parameter keeps its own value';
is $n, 99, 'the write reached the file-scope variable';

my $state-like = 0;
class B {
    has $.v;
    submethod TWEAK(:$v) { $OUTERS::state-like = $v }
}
B.new(:v(7));
is $state-like, 7, 'a submethod hands a value to the file scope';

my @list = 1, 2;
class C { method push($list) { @OUTERS::list.push: $list; @OUTERS::list.elems } }
is C.new.push(3), 3, 'array through OUTERS from a method';

my %h = a => 1;
sub hs() { %OUTERS::h.elems }
is hs(), 1, '%OUTERS:: read from a sub';
sub as() { @OUTERS::list.elems }
is as(), 3, '@OUTERS:: read from a sub';
