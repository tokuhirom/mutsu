use Test;

plan 7;

{
    sub lightning {...}
    throws-like 'lightning()', X::StubCode, 'stub routine dies as X::StubCode';
    sub lightning {42}
    is lightning(), 42, 'stub routine can be redefined without supersede';
}

{
    throws-like 'sub hail {26}; sub hail {10}', X::Redeclaration,
      'non-stub routine redeclaration dies';
}

{
    use MONKEY-TYPING;
    sub hail {26}
    supersede sub hail {8}
    is hail(), 8, 'supersede redefines non-stub routine';
}

{
    {
        sub scoped {41}
        is scoped(), 41, 'first block-local routine works';
    }
    {
        sub scoped {99}
        is scoped(), 99, 'second block-local routine does not conflict';
    }
}

{
    sub warny {???}
    lives-ok { warny() }, 'admonitory stub routine lives';
}
