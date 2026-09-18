use v6;
use Test;

# `$s ~= ...` on a local compiles to a single `OpCode::ConcatAssignLocal`
# (#8695), which reads the slot AFTER the RHS and appends into the existing
# buffer when that buffer is the value's alone. Growing a buffer in place is
# only correct under Raku's value semantics if every way of holding a second
# reference to the same string is detected, so that is most of what this pins,
# alongside the normalization rule that keeps the grown buffer NFC (#8725).

plan 40;

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
    # composes with the last character of the accumulated string, so the
    # append has to renormalize the join rather than push the mark on -- or
    # `.chars` and comparison would silently change.
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

# -- non-ASCII appends stay linear AND stay normalized (#8725) ---------------

{
    # The in-place path takes a non-ASCII suffix now, so long accumulations of
    # one must still come out with every character and nothing composed.
    my $s = '';
    $s ~= "\c[SNOWMAN]" for ^5000;
    is $s.chars, 5000, 'a long non-ASCII accumulation keeps every character';
    is $s.substr(0, 1), "\c[SNOWMAN]", 'and starts with what was appended first';
    is $s.substr(*-1), "\c[SNOWMAN]", 'and ends with what was appended last';
}

{
    # A suffix that is not itself NFC must be normalized before it lands.
    my $s = 'x';
    $s ~= "e\x[301]";
    is $s.chars, 2, 'a non-NFC suffix is normalized on the way in';
    is $s.ords.join(','), '120,233', 'to the composed codepoint';
}

{
    # Repeated composing appends: each one composes with the tail left by the
    # previous, so the window rule is exercised on a buffer it just built.
    my $s = '';
    $s ~= "e\x[301]" for ^100;
    is $s.chars, 100, 'repeated composing appends stay one grapheme each';
    is $s.ords.elems, 100, 'with no leftover combining marks';
}

{
    # A below-mark (ccc 220) arriving after an above-mark (ccc 230) has to be
    # reordered in front of it -- and once it is, it composes with the starter
    # that the window had to reach back to.
    my $s = "a\x[30A]";
    $s ~= "\x[323]";
    is $s.ords.join(','), '7841,778', 'canonical reordering happens across the join';
    is $s.chars, 1, 'and the result is still one grapheme';
}

{
    # Hangul composes starter-with-starter, so "is a starter" is not a
    # sufficient test for skipping normalization.
    my $s = "\x[1100]";
    $s ~= "\x[1161]";
    is $s.ords.join(','), '44032', 'Hangul L + V compose across the join';
    $s ~= "\x[11A8]";
    is $s.ords.join(','), '44033', 'and LV + T compose too';
}

{
    # NFC_QC = No: the character is replaced by normalization even though it
    # is a starter, so it cannot be appended verbatim.
    my $s = 'x';
    $s ~= "\x[212B]";
    is $s.ords.join(','), '120,197', 'a singleton is normalized rather than appended as-is';
}

{
    # An unbounded combining run has no interior normalization boundary; the
    # append has to stay correct where the bounded window gives up.
    my $s = 'a';
    $s ~= "\x[334]" for ^80;
    $s ~= "\x[301]";
    is $s.chars, 1, 'a very long combining run is still one grapheme';
    # The acute is not blocked by the ccc-1 overlays, so it composes with the
    # starter: 'a' + 301 -> U+00E1, leaving 80 overlays behind it.
    is $s.ords.elems, 81, 'and keeps every mark';
    is $s.ords[0], 225, 'with the acute composed onto the starter';
}
