use Test;

plan 21;

# ADR-0048 Phase 4 (D4/D5): what a `while`/`until`/`repeat` loop supplies to
# its body's placeholder parameters.
#
# The rule (verified against real `raku` -- this file passes unmodified under
# rakudo as well as under mutsu): a prefix `while`/`until` BLOCK is a Block the
# loop invokes with ONE argument, the *raw* (un-boolified) value of the
# condition as WRITTEN, re-supplied on every pass. `repeat {} while/until`
# invokes its body once before the condition has ever run, so its first pass is
# supplied `Mu`. A `while`/`until` statement MODIFIER introduces no block at
# all, so its placeholders stay the enclosing routine's own parameters.

sub arity-fails($code, $expected, $desc) {
    my $msg = 'did not die';
    try {
        EVAL $code;
        CATCH { default { $msg = .message } }
    }
    is $msg, $expected, $desc;
}

my $expect1 = 'Too few positionals passed; expected 1 argument but got 0';
my $expect-two-of-one = 'Too few positionals passed; expected 2 arguments but got 1';

# --- the RAW condition value, not the boolified one ---

{
    my $got;
    while 42 { $got = $^c; last }
    is $got, 42, 'while supplies the raw condition value, not True';
}

{
    my $got;
    until False { $got = $^c; last }
    is $got, False, 'until supplies the value of the condition as WRITTEN, not its negation';
}

{
    my $got;
    until 0 { $got = $^c; last }
    is $got, 0, 'until does not leak the parser negation into the supplied value';
}

{
    my $got;
    my $x = 5;
    while !$x { $got = $^c; last }
    my $never = $got.defined;
    nok $never, 'a hand-written `while !EXPR` that is false never invokes the body';
}

{
    my @got;
    my $i = 0;
    while $i++ < 3 { @got.push($^c) }
    is @got.join(','), 'True,True,True',
        'the supplied value is re-evaluated every pass (here the comparison result)';
}

{
    my @got;
    my $i = 0;
    while $i < 3 { @got.push($^c); $i++ }
    is @got.join(','), 'True,True,True', 'and stays the raw per-iteration condition value';
}

# --- the placeholder is the BODY's parameter, never the enclosing block's ---

is { while 42 { $^c; last } }.arity, 0,
    'a while body placeholder does not leak into the enclosing block signature';

is { until False { $^c; last } }.arity, 0, 'nor does an until body placeholder';

{
    sub while-ph-arity { while 42 { $^c; last }; 99 }
    is &while-ph-arity.arity, 0, 'nor into an enclosing routine';
    is while-ph-arity(), 99, 'and that routine still takes no arguments';
}

# --- one value supplied, so a second placeholder is an arity failure ---

arity-fails 'while 42 { "$^a $^b"; last }', $expect-two-of-one,
    'a while body declaring two positionals is an arity failure';

arity-fails 'until False { "$^a $^b"; last }', $expect-two-of-one,
    'the same for until';

{
    my $ran = False;
    while 0 { $ran = "$^a $^b" }
    nok $ran, 'the arity failure is raised on INVOCATION, so a never-entered loop is silent';
}

# --- a block genuinely nested inside the loop braces is a SECOND block, and
#     the loop supplies it nothing ---

arity-fails 'my $i = 0; while $i++ < 2 { { $^a } }', $expect1,
    'a bare block nested in a while body is a separate zero-argument Block';

# --- repeat: `Mu` on the first pass, the condition value afterwards ---

{
    my @got;
    my $i = 0;
    repeat { @got.push($^c.gist) } while $i++ < 2;
    is @got.join(','), '(Mu),True,True',
        'repeat while supplies Mu on the first pass, then the condition value';
}

{
    my @got;
    my $i = 0;
    repeat { @got.push($^c.gist) } until $i++ > 1;
    is @got.join(','), '(Mu),False,False',
        'repeat until supplies the value of the condition as WRITTEN';
}

arity-fails 'my $i = 0; repeat { "$^a $^b" } while $i++ < 1', $expect-two-of-one,
    'a repeat body declaring two positionals is an arity failure';

# --- D5: an explicit signature wins over a placeholder ---

sub sig-clash-fails($code, $desc) {
    my $msg = 'did not die';
    try {
        EVAL $code;
        CATCH { default { $msg = .message } }
    }
    ok $msg.contains("cannot override existing signature"), $desc;
}

sig-clash-fails 'while 42 -> $x { $^c; last }',
    'a placeholder cannot override a while pointy signature';

sig-clash-fails 'until False -> $x { $^c; last }',
    'nor an until pointy signature';

sig-clash-fails 'my $i = 0; repeat while $i++ < 1 -> $x { $^c }',
    'nor a repeat pointy signature';

# --- a while/until STATEMENT MODIFIER introduces no block ---

{
    my $i = 0;
    my @got;
    sub while-modifier-ph { @got.push("$^a") while $i++ < 2; }
    while-modifier-ph(7);
    is @got.join(','), '7,7',
        'a while modifier placeholder is the enclosing routine parameter, not the condition';
}
