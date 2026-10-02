use Test;

# #10626: a nested block's `MY::` is that block's own pad. Routines declared
# or imported in an enclosing scope are not in it; the block's own
# declarations and its own `use`s are.

plan 12;

sub outer-sub { 1 }

is MY::<&outer-sub>.name, 'outer-sub', 'file-scope MY:: lists a file-scope sub';

{
    is-deeply MY::<&outer-sub>, Nil, 'an enclosing sub is not in a nested MY::';
    is-deeply MY::<&plan>, Nil, 'an enclosing import is not in a nested MY::';

    sub inner-sub { 2 }
    my sub inner-my-sub { 3 }
    is MY::<&inner-sub>.name, 'inner-sub', 'a sub the block declares is listed';
    is MY::<&inner-my-sub>.name, 'inner-my-sub', 'a my sub the block declares is listed';
    is-deeply MY::.keys.grep(*.starts-with('&')).sort.List,
        <&inner-my-sub &inner-sub>,
        'the nested MY:: holds exactly the block-declared routines';
}

{
    use Test;
    is MY::<&plan>.name, 'plan', "a block's own use is listed in its MY::";
    is-deeply MY::<&outer-sub>, Nil, 'an enclosing sub stays out of an importing block';
    for 1 {
        is-deeply MY::<&plan>, Nil, "a loop body does not list its parent block's import";
    }
    {
        use Test;
        is MY::<&ok>.name, 'ok', 'a re-import in a deeper block is listed there';
    }
    is MY::<&ok>.name, 'ok', 'the outer importing block still lists its import';
}

{
    if True {
        is-deeply MY::<&outer-sub>, Nil, 'an if body does not list enclosing routines';
    }
}
