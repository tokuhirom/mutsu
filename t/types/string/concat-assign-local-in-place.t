use v6;
use Test;

# `$s ~= ...` on a local compiles to a single `OpCode::ConcatAssignLocal`
# (#8695), which reads the slot AFTER the RHS and appends into the existing
# buffer when that buffer is the value's alone. Growing a buffer in place is
# only correct under Raku's value semantics if every way of holding a second
# reference to the same string is detected, so that is most of what this pins,
# alongside the normalization rule that lets the append skip NFC.

plan 25;

# -- the accumulation itself -------------------------------------------------

{
    my $s = '';
    $s ~= 'x' for ^5;
    is $s, 'xxxxx', 'repeated appends accumulate';
    is $s.chars, 5, 'and .chars agrees';
}

{
    my $s = 'a';
    $s ~= 'b';
    $s ~= 'cd';
    is $s, 'abcd', 'appends of different lengths';
}

{
    # Long enough that a quadratic implementation would be obvious, and that a
    # buffer-reuse bug would show up as a wrong length rather than wrong text.
    my $s = '';
    $s ~= 'ab' for ^10000;
    is $s.chars, 20000, 'a long accumulation keeps every character';
    is $s.substr(0, 4), 'abab', 'and starts with what was appended first';
    is $s.substr(*-4), 'abab', 'and ends with what was appended last';
}

# -- aliasing: the in-place append must never be visible through a second
#    holder of the same string ------------------------------------------------

{
    my $a = 'shared';
    my $b = $a;
    $a ~= '!';
    is $a, 'shared!', 'the appended-to variable sees the append';
    is $b, 'shared', 'a plain copy taken before the append does not';
}

{
    my $a = 'x';
    my @kept;
    for ^3 {
        @kept.push($a);
        $a ~= 'y';
    }
    is-deeply @kept, ['x', 'xy', 'xyy'], 'values pushed into an array keep their own text';
    is $a, 'xyyy', 'and the accumulator is unaffected by having been pushed';
}

{
    my $a = 'k';
    my %h = value => $a;
    $a ~= 'z';
    is %h<value>, 'k', 'a copy stored in a hash is not grown by a later append';
}

{
    my $a = 'bound';
    my $alias := $a;
    $a ~= '+';
    is $alias, 'bound+', 'a `:=` alias sees the append (it IS the same container)';
}

# -- normalization -----------------------------------------------------------

{
    # NFC is not append-closed: a combining mark at the start of the suffix
    # composes with the last character of the accumulated string. The fast
    # path must decline this (it requires an ASCII suffix), or `.chars` and
    # comparison would silently change.
    my $s = "e";
    $s ~= "\x[301]";
    is $s.chars, 1, 'a combining mark composes with the character before it';
    is $s.ords.join(','), '233', 'and the result is the composed codepoint';
}

{
    my $s = "e\x[301]";
    $s ~= "x";
    is $s.ords.join(','), '233,120', 'appending ASCII after a composed character composes nothing';
    is $s.chars, 2, 'and leaves two graphemes';
}

{
    my $s = 'ascii';
    $s ~= "\c[SNOWMAN]";
    is $s.chars, 6, 'an append may take an ASCII string non-ASCII';
    is $s, "ascii\c[SNOWMAN]", 'with the right text';
}

# -- shapes the fused op must hand back to the general path ------------------

{
    my $s;
    $s ~= 'seeded';
    is $s, 'seeded', 'an undefined LHS seeds the empty string (METAOP_ASSIGN identity)';
}

{
    my $s = 'n';
    $s ~= 42;
    is $s, 'n42', 'a non-Str RHS stringifies';
}

{
    my $s = 'ab';
    $s ~= $s;
    is $s, 'abab', 'self-append reads the old value';
}

{
    # rakudo reads the left operand AFTER the right: a RHS that assigns to the
    # accumulator wins, rather than being overwritten by a pre-read value.
    my $s = 'a';
    sub bump() { $s = 'ZZZ'; 'b' }
    $s ~= bump();
    is $s, 'ZZZb', 'the LHS is read after the RHS, as rakudo does';
}

{
    sub readonly-append($x) { $x ~= 'nope' }
    dies-ok { readonly-append('ro') }, 'appending to a readonly parameter still dies';
}

{
    # A captured lexical is held by the closure's env as well as the slot, so
    # the append cannot own the buffer -- it must still be correct.
    my $s = 'cap';
    my $peek = { $s };
    $s ~= 'tured';
    is $s, 'captured', 'a captured accumulator still appends';
    is $peek(), 'captured', 'and the closure reads the current value';
}
