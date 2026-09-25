use v6;
use Test;

# `say`, `put`, `print` and `note` share one renderer, whether they are called
# as a listop (an opcode) or as a routine (`&say(...)`, an alias). They used to
# be five bodies applying different subsets of the pre-output checks, so
# `note 1/0` and `&put(1/0)` printed `Inf` while every other form died, and
# only `say` threw on an unhandled Failure (#9449).

plan 20;

# Send the output somewhere harmless: these tests are about what dies.
my $*OUT = class { method print(*@) { True }; method say(*@) { True } }.new;
my $*ERR = $*OUT;

for <say put print note> -> $name {
    my &listop = EVAL "-> \\v \{ $name v }";
    my &routine = EVAL "-> \\v \{ \&$name\(v) }";

    throws-like { listop(1/0) }, X::Numeric::DivideByZero,
        "$name 1/0 dies";
    throws-like { routine(1/0) }, X::Numeric::DivideByZero,
        "\&$name\(1/0) dies";
    throws-like { listop(Failure.new("boom")) }, Exception, message => 'boom',
        "$name on an unhandled Failure throws it";
    throws-like { routine(Failure.new("boom")) }, Exception, message => 'boom',
        "\&$name on an unhandled Failure throws it";
    lives-ok {
        my $f = Failure.new("handled");
        $f.so;    # marks it handled
        listop($f);
    }, "$name on a handled Failure renders it";
}
