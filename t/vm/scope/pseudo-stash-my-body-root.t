use Test;

# #10849: a routine's (or closure's) own top-level `MY::` is that body's pad.
# Routines declared or imported in an enclosing scope are not in it; the
# body's own declarations (hoisted, so visible before their text) and its own
# `use`s are. `LEXICAL::` and `OUTER::MY::` still reach the enclosing ones.

plan 15;

sub outer-sub { 1 }

sub plain-routine {
    is-deeply MY::<&outer-sub>, Nil, 'an enclosing sub is not in a routine MY::';
    is-deeply MY::<&plan>, Nil, 'an enclosing import is not in a routine MY::';
    is LEXICAL::<&outer-sub>.name, 'outer-sub', 'LEXICAL:: still sees an enclosing sub';
    is OUTER::MY::<&outer-sub>.name, 'outer-sub', 'OUTER::MY:: reaches the unit pad';
}
plain-routine;

sub declaring-routine {
    is MY::<&later-sub>.name, 'later-sub', 'a routine declared later in the body is listed';
    sub later-sub { 2 }
    is-deeply MY::.keys.grep(*.starts-with('&')).sort.List, ('&later-sub',),
        'the routine MY:: holds exactly the routines the body declares';
}
declaring-routine;

sub importing-routine {
    use Test;
    is MY::<&plan>.name, 'plan', "a routine's own use is listed in its MY::";
    is-deeply MY::<&outer-sub>, Nil, 'an enclosing sub stays out of an importing routine';
    {
        is-deeply MY::<&plan>, Nil, "a nested block does not list its routine's import";
    }
}
importing-routine;

my &closure = {
    is-deeply MY::<&outer-sub>, Nil, 'an enclosing sub is not in a closure MY::';
    is-deeply MY::.keys.grep(*.starts-with('&')).List, (),
        'a closure declaring no routine has none in its MY::';
};
closure();

lives-ok {
    die 'leaked' if MY::<&outer-sub>.defined;
}, 'a block passed to a routine does not list enclosing subs';

class C {
    method m { MY::<&outer-sub> }
}
is-deeply C.m, Nil, 'an enclosing sub is not in a method MY::';

is MY::<&outer-sub>.name, 'outer-sub', 'file-scope MY:: still lists a file-scope sub';
is MY::<&plan>.name, 'plan', 'file-scope MY:: still lists a file-scope import';
