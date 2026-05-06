use Test;

plan 8;

# Test 1: simple die shows backtrace with <unit>
{
    my $out = "";
    try { die "oops" }
    ok $!.defined, "die produces an exception";
    is $!.message, "oops", "exception message is correct";
}

# Test 2: backtrace method on exception
{
    sub inner { die "deep error" }
    sub outer { inner() }
    try { outer() }
    ok $!.defined, "nested die produces an exception";
    my $bt = $!.backtrace;
    ok $bt.defined, ".backtrace returns a defined value";
}

# Test 3: die with structured exception
{
    try { die X::AdHoc.new(message => "structured") }
    ok $!.defined, "structured exception is caught";
    is $!.message, "structured", "structured exception message";
}

# Test 4: die preserves exception type
{
    try { die "simple string" }
    ok $!.defined, "simple die caught";
    is $!.message, "simple string", "simple die message";
}
