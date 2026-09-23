use v6;
use Test;

# #9141: string appends outside the plain-local `~=` used to copy the whole
# accumulated string and re-run NFC over it on every append. They now append in
# place: `%h<k> ~=` / `@a[$i] ~=` through the fused element store, `$y = $y ~ x`
# and a `given`/`when` local through `ConcatAssignLocal`, and every other `~`
# normalizes only the join. This pins that none of that is observable except
# as speed: aliases keep their own text, the stores keep their store rules, and
# the result stays NFC.

plan 35;

# -- hash elements -----------------------------------------------------------

{
    my %h = k => '';
    %h<k> ~= 'ab' for ^5000;
    is %h<k>.chars, 10000, 'a hash element accumulates every append';
    is %h<k>.substr(*-2), 'ab', 'and ends with the last one';
}

{
    my %h = k => 'shared';
    my $copy = %h<k>;
    %h<k> ~= '!';
    is %h<k>, 'shared!', 'the element sees the append';
    is $copy, 'shared', 'a copy read out of the element before it does not';
}

{
    my %h = k => 'x';
    my %g = %h;
    %h<k> ~= 'y';
    is %h<k>, 'xy', 'the appended hash changes';
    is %g<k>, 'x', 'a copy of the whole hash does not';
}

{
    my %h;
    %h<new> ~= 'v';
    is %h<new>, 'v', 'a missing key is vivified and seeded with the empty string';
}

{
    my %h = k => 'e';
    %h<k> ~= "\x[301]";
    is %h<k>.ords.join(','), '233', 'a combining mark composes across the join in an element';
}

{
    my %h = k => 'n';
    %h<k> ~= 42;
    is %h<k>, 'n42', 'a non-Str RHS stringifies';
}

{
    my Str %h = k => 'typed';
    %h<k> ~= '!';
    is %h<k>, 'typed!', 'a typed hash still appends';
}

{
    my %h = k => 'a';
    my $alias := %h<k>;
    %h<k> ~= 'b';
    is $alias, 'ab', 'a `:=`-bound element is written through';
}

{
    my %h = k => 'ab';
    %h<k> ~= %h<k>;
    is %h<k>, 'abab', 'an element appended to itself reads the old value';
}

# -- array elements ----------------------------------------------------------

{
    my @a = '';
    @a[0] ~= "\c[SNOWMAN]" for ^5000;
    is @a[0].chars, 5000, 'an array element accumulates every non-ASCII append';
}

{
    my @a = 'x';
    my @b = @a;
    @a[0] ~= 'y';
    is-deeply [@a, @b], [['xy'], ['x']], 'a copy of the array keeps its own element';
}

{
    my @a = 'k';
    my $kept = @a[0];
    @a[0] ~= 'z';
    is $kept, 'k', 'a copy read out of the element keeps its text';
}

{
    my @a;
    @a[3] ~= 'z';
    is @a.raku, '[Any, Any, Any, "z"]', 'an element past the end is vivified';
}

{
    my Int @a = 1;
    throws-like { @a[0] ~= 'x' }, X::TypeCheck::Assignment,
        'a type-constrained element still refuses a Str';
}

{
    my @a = 'a', 'b';
    my $i = 0;
    @a[$i++] ~= '!';
    is-deeply @a, ['a!', 'b'], 'the index is evaluated once';
    is $i, 1, 'exactly once';
}

# -- `$y = $y ~ x` -----------------------------------------------------------

{
    my $y = '';
    $y = $y ~ 'ab' for ^5000;
    is $y.chars, 10000, 'a plain re-assignment accumulates every append';
}

{
    my $a = 'ab';
    my $b = $a;
    $a = $a ~ 'c';
    is "$a $b", 'abc ab', 'a copy taken before the re-assignment is not grown';
}

{
    my $u;
    my $warned = 0;
    {
        CONTROL { when CX::Warn { $warned++; .resume } }
        $u = $u ~ 'x';
    }
    is $u, 'x', 'an undefined LHS stringifies to the empty string';
    is $warned, 1, 'and still warns, unlike `~=`';
}

{
    # rakudo passes the container to `~` and reads it inside the call, i.e.
    # after the RHS ran.
    my $z = 'a';
    $z = $z ~ do { $z = 'q'; 'r' };
    is $z, 'qr', 'the left operand is read after the RHS, as in rakudo';
}

{
    my $s = 'e';
    $s = $s ~ "\x[301]";
    is $s.chars, 1, 'the re-assignment normalizes across the join';
}

# -- a local declared inside given/when (mirrored to env) --------------------

{
    my $out;
    given 1 {
        when 1 {
            my $x = '';
            $x ~= 'ab' for ^5000;
            $out = $x;
        }
    }
    is $out.chars, 10000, 'a given/when local accumulates every append';
}

{
    my $seen;
    given 1 {
        when 1 {
            my $x = 'c';
            my &peek = { $x };
            $x ~= 'd';
            $seen = peek();
        }
    }
    is $seen, 'cd', 'a closure reading the given/when local sees the append';
}

# -- plain infix `~` ---------------------------------------------------------

{
    my $left = 'abc';
    my $r = $left ~ 'def';
    is "$left $r", 'abc abcdef', 'infix `~` never grows an operand still held elsewhere';
}

{
    is ("e" ~ "\x[301]").ords.join(','), '233', 'infix `~` composes across the join';
    is ("\x[1100]" ~ "\x[1161]").ords.join(','), '44032', 'including Hangul L + V';
    is ("x" ~ "e\x[301]").ords.join(','), '120,233', 'and normalizes a non-NFC right operand';
    my $long = "\c[SNOWMAN]" x 1000;
    is ($long ~ $long).chars, 2000, 'two long non-ASCII operands concatenate';
}

# -- infix `x` ---------------------------------------------------------------

{
    is ("\c[SNOWMAN]" x 3).chars, 3, 'a non-ASCII operand repeats';
    is ("\x[301]" x 2).ords.join(','), '769,769', 'a lone combining mark repeats as marks';
    is ("e\x[301]" x 2).ords.join(','), '233,233', 'a composed character repeats composed';
}
