use Test;

use lib 't/lib';

plan 6;

# `now` / `time` are CORE terms, so with nothing else in scope their call form
# `now()` is a compile-time "Undeclared routine". A routine of the same name
# that is imported or declared in an enclosing scope shadows the term, and the
# call form is then an ordinary routine call (#10369). The undeclared case is
# pinned in t/lang/term-keyword-call-form-undeclared.t, away from these imports.

{
    use NowTimeShadowFixture;

    is now(), 'imported now', 'an imported now routine is callable as now()';
    is now, 'imported now', 'the bare now term resolves to the imported routine';
    is time(), 42, 'an imported time routine is callable as time()';
    is time, 42, 'the bare time term resolves to the imported routine';
}

{
    sub now { 'local now' }
    is now(), 'local now', 'a locally declared now sub is callable as now()';
    sub time { 7 }
    is time() + 1, 8, 'a locally declared time sub is callable as time()';
}
