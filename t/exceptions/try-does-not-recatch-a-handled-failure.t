use Test;

# A `try` whose body yields a *live* Failure catches it: the Failure becomes
# handled and `$!` holds its exception. One that yields a Failure something has
# ALREADY handled has nothing to catch -- by then it is an ordinary value -- so
# `$!` must stay undefined.

plan 7;

# Live Failure: caught.
{
    try { "foo"[2] };
    isa-ok $!, X::OutOfRange, 'a live Failure as the try value is caught';
}

# Handled Failure: not caught.
for <defined Bool so> -> $handler {
    my $f = "foo"[2];
    $f."$handler"();
    try { $f };
    nok $!.defined, "a Failure handled by .$handler is not re-caught";
}

# The same one call deep, which is the shape `lives-ok` uses
# (`try { $code(); }` in rakudo's Test.rakumod).
{
    my &c = { my $f = "foo"[2]; $f.defined; $f };
    try { c() };
    nok $!.defined, 'nor is one returned from a called block';
}

# A try that yields an ordinary value still clears $!.
{
    try { "foo"[2] };
    try { 42 };
    nok $!.defined, 'a successful try clears $!';
}

# And a real throw is still caught.
{
    try { die "boom" };
    is $!.message, 'boom', 'a thrown exception is still caught';
}
