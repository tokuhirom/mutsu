use v6;
use Test;

# Every spelling of a comparison-family infix is the one operator (#9447).
#
# `a OP b` compiles to an opcode, while `[OP]`, `»OP«`, `ZOP`, `XOP` and
# `&infix:<OP>(a, b)` used to fall back to a separate table that carried its
# own copy of each operator. The copies drifted: `[before] 10, 9` compared the
# numbers as strings and answered True, and `[eq] Any, ""` stringified the type
# object differently from `Any eq ""`. Now every form runs the opcode's body,
# so for each operand pair all forms must agree with the plain infix -- and
# the plain infix must agree with Rakudo, pinned in %expected below.

my @pairs =
    (10, 9), (9, 10), (2, 10), (10, 10), ("10", "9"), ("b", "a"), ("a", "a"),
    (1.5, 1.25), (-1, 1), (Any, ""), ("", Any), (True, 1);

my @names = <eq ne lt gt le ge leg before after === min max>;

# Each form is compiled from the literal operator spelling, so it exercises
# exactly the path a program writing `[before]` or `Zeq` takes. (`[leg]` is
# not a legal reduction in Raku -- `leg` is not chaining -- so it has none.)
sub forms-for($name) {
    my %f =
        infix   => EVAL('-> $a, $b { $a ' ~ $name ~ ' $b }'),
        routine => EVAL('-> $a, $b { &infix:<' ~ $name ~ '>($a, $b) }'),
        zip     => EVAL('-> $a, $b { (($a,) Z' ~ $name ~ ' ($b,))[0] }'),
        cross   => EVAL('-> $a, $b { (($a,) X' ~ $name ~ ' ($b,))[0] }'),
        hyper   => EVAL('-> $a, $b { (($a,) »' ~ $name ~ '« ($b,))[0] }');
    %f<reduce> = EVAL('-> $a, $b { [' ~ $name ~ '] $a, $b }') unless $name eq 'leg';
    %f
}

# Rakudo's answers for the plain infix, for the pairs whose answer the old
# duplicate got wrong or that exercise a coercion edge.
my @expected =
    ('before', 10,  9,   False),
    ('before', 9,   10,  True),
    ('before', 2,   10,  True),
    ('after',  2,   10,  False),
    ('after',  10,  9,   True),
    ('eq',     Any, "",  True),
    ('eq',     "",  Any, True),
    ('ne',     Any, "",  False),
    ('leg',    "b", "a", More),
    ('lt',     "10", "9", True),
    ('min',    10,  9,   9),
    ('max',    2,   10,  10);

sub show($v) { $v.defined ?? $v.raku !! $v.^name }

plan @names.elems * @pairs.elems + @expected.elems;

my %forms-of = @names.map({ $_ => forms-for($_) });

for @names -> $name {
    my %forms := %forms-of{$name};
    for @pairs -> ($a, $b) {
        my $want = quietly %forms<infix>($a, $b);
        my @bad;
        for %forms.keys.sort -> $form {
            my $got = quietly %forms{$form}($a, $b);
            @bad.push("$form gave {show $got}") unless $got eqv $want;
        }
        ok !@bad, "{show $a} $name {show $b}: every form agrees with {show $want}"
            ~ (@bad ?? " ({@bad.join('; ')})" !! "");
    }
}

for @expected -> ($name, $a, $b, $want) {
    is-deeply (quietly %forms-of{$name}<infix>($a, $b)), $want,
        "{show $a} $name {show $b} is {show $want}, as in Rakudo";
}
