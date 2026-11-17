use Test;

throws-like { { fail "q" }() }, X::AdHoc,
    message => "q", 'a mainline block throws at its fail site';

sub call-block(&b) { b() }
throws-like { call-block({ fail "z" }) }, X::AdHoc,
    message => "z", 'a mainline block passes through a called routine';

sub catch-block(&b) {
    CATCH { default { return "caught" } }
    b()
}
is catch-block({ fail "handled" }), "caught",
    'a called routine can catch a mainline block failure';

sub outer {
    sub inner(&b) { b() }
    inner({ fail "soft" })
}
ok outer() ~~ Failure, 'a block lexically in a routine fails that routine';

done-testing;
