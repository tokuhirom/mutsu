use Test;

plan 6;

# Pin for roast/S06-advanced/return-prioritization.t: a `return` raised inside
# a LEAVE phaser overrides an in-flight return that targets the same routine,
# and a closure called in LEAVE can redirect the unwind to an outer routine,
# but an in-flight return to an outer routine is not interruptible.

{
    sub s() {
        LEAVE return 1;
        return 0;
    }
    is s(), 1, "LEAVE return overrides an explicit return of the same routine";
}

{
    sub s() {
        LEAVE return 1;
        0;
    }
    is s(), 1, "LEAVE return overrides the implicit return value";
}

{
    my $val = 0;
    sub s(&code) {
        LEAVE return 2;
        code();
    }
    sub s2(&code) {
        s(&code);
        $val = 1;
    }
    sub s3() {
        s2({ return 1 });
        return 0;
    }
    is s3(), 1, "LEAVE return cannot interrupt a return unwinding to an outer routine";
    is $val, 0, "the outer return keeps unwinding past the LEAVE frame";
}

{
    sub s(&code) {
        return 0;
        LEAVE { code() }
    }
    sub s2() {
        s({ return 1 });
        return 2;
    }
    is s2(), 1, "closure called in LEAVE redirects the unwind to its own routine";
}

{
    my $val;
    sub inner() { return 5 }
    sub s1(&code) {
        code();
        LEAVE { $val = inner }
    }
    sub s2() {
        s1({ return 1 });
        return 2;
    }
    is s2(), 1, "returns fully caught inside a LEAVE block don't disturb the unwind";
}
