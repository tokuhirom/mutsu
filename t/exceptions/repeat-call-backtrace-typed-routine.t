use Test;

plan 9;

# A routine that runs as a TRIR chunk once its call is cached (a typed or
# parameterless one) reports the line its `die` is written on, not the line
# of the statement that called it, on every call (#18080). TRIR adopts each
# call site's line before the call, as the VM does at a call instruction.

sub typed(Int $x --> Int) { die "boom" }   # line 10
sub bare() { die "bare" }                 # line 11
sub two(Int $x) {
    my $y = $x + 1;
    die "two $y"                          # line 14
}

for 1..3 -> $i {
    try { typed(1) };
    is $!.backtrace.list.first({ .is-routine && .subname eq 'typed' }).line, 10,
        "call $i: a typed routine's frame names its die's line";
    try { bare() };
    is $!.backtrace.list.first({ .is-routine && .subname eq 'bare' }).line, 11,
        "call $i: a parameterless routine's frame names its die's line";
    try { two(1) };
    is $!.backtrace.list.first({ .is-routine && .subname eq 'two' }).line, 14,
        "call $i: a die on a later line of the body";
}
