# A `-> |c { ... }` closure stored at file scope and called from a method:
# the capture's elements are the CALLING method's variables (#18253).
unit class CaptureClosureCallerLexical;

my &first-of = -> |c { c.list[0] };
my &bump = -> |c { c.list[0]++ };

method pass-own(--> Int) {
    my $e = 42;
    first-of($e)
}

method write-own(--> Int) {
    my $e = 1;
    bump($e);
    $e
}
