use Test;

# GH-15259 (annotations 0.1.0): a package stash lists its `our` operator
# routines, so code copying a stash (`OUR.WHO.BIND-KEY: .key, .value for P::`)
# sees them.
plan 4;

module P {
    our sub f { 1 }
    our sub infix:<zz>($a, $b) { "zz$a$b" }
    our only prefix:<yy>($a) { "yy$a" }
}
is P::.keys.sort.join(' '), '&f &infix:<zz> &prefix:<yy>', 'operator routines are stash members';
is P::<&infix:<zz>>(1, 2), 'zz12', 'and callable through it';
my %copy;
%copy{.key} = .value for P::;
is %copy.elems, 3, 'a stash can be copied key by key';
is %copy{'&prefix:<yy>'}(5), 'yy5', 'the copied operator works';
