use Test;

plan 11;

module A {
    our proto sub pr(|) {*}
    multi sub pr(@v) { 'a' }
    multi sub pr(Str $s) { 's' }
    our sub plain() { 1 }
}

is A::<&pr>.^name, 'Sub', 'a proto in its package stash is a Sub';
is A::<&plain>.^name, 'Sub', 'a plain routine in its package stash is a Sub';
is &A::pr.^name, 'Sub', 'a qualified routine reference is a Sub';
is &A::pr.name, 'pr', 'a qualified routine exposes its unqualified name';
ok A::<&pr> === &A::pr, 'stash and qualified proto references share identity';
ok A::<&plain> === &A::plain, 'stash and qualified plain references share identity';

my &p = A::<&pr>;
is p([1]), 'a', 'a proto read from the stash dispatches to its array candidate';
is p('x'), 's', 'a proto read from the stash dispatches to its string candidate';
is A::<&plain>(), 1, 'a plain routine read from the stash remains callable';
is A::.AT-KEY('&pr').^name, 'Sub', 'the whole stash agrees on the routine type';
ok A::.AT-KEY('&pr') === A::<&pr>, 'whole and keyed stash reads share identity';
