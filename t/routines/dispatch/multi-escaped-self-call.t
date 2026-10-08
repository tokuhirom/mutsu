use Test;

plan 3;

# A lexical multi reached through an escaped `&name` once its declaring scope
# is gone must still be able to call its own family by name (#12321).
sub o7 {
    multi sub b(Int $n) { $n > 0 ?? b($n - 1) + 1 !! 0 }
    multi sub b(Str $n) { 7 }
    &b
}
is o7()(3), 3, 'recursive call through the escaped dispatcher';
is o7()("x"), 7, 'other candidate still dispatches';

sub mk {
    my %store;
    multi sub ST($p, Int:D $v) { %store<n> = $v; $v }
    multi sub ST($p, Str:D $v) { ST($p, $v.chars) }
    &ST
}
is mk()(Nil, "abcd"), 4, 'a sibling candidate calls another by name';
