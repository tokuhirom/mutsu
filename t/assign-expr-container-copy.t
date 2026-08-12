use v6;
use Test;

plan 8;

# Expression-position container assignment (`cond and @a = @b`) must follow
# Raku `=` copy semantics: the target owns a DISTINCT container, so mutating
# it later cannot leak into the source (and vice versa). Pins the Text::CSV
# `@!ahead` corruption: `$!io and @ch = @!ahead; @!ahead = (); @ch.append(...)`
# filled the attribute back up because `@ch` had adopted its backing store.

{
    my @src = 1, 2, 3;
    my @ch;
    True and @ch = @src;
    @src.push(9);
    is-deeply @ch, [1, 2, 3], 'expression-position @a = @b copies (source push invisible)';
    @ch.push(10);
    is-deeply @src, [1, 2, 3, 9], 'target push does not leak into the source';
}

{
    my %src = a => 1;
    my %h;
    True and %h = %src;
    %src<b> = 2;
    is-deeply %h, { a => 1 }, 'expression-position %a = %b copies';
}

# The attribute-array shape of the same bug, through a re-entrant method call
# (Text::CSV's skip_empty_rows recursion).
class Buf1 {
    has Str @!ahead;
    method go (Int $depth --> Bool) {
        my @ch;
        True and @ch = @!ahead;
        @!ahead = ();
        @ch.append("d" ~ $depth, "x");
        $depth == 0 and return self.go(1);
        True;
    }
    method peek { @!ahead.elems }
}
{
    my $b = Buf1.new;
    $b.go(0);
    is $b.peek, 0, 'attribute array stays empty after expression-position copy + append';
}

# Assignment as `and`-RHS still assigns and yields the value.
{
    my @a;
    my $ok = (True and @a = <x y>);
    is-deeply @a, [<x y>], 'and-RHS assignment stores the value';
    is-deeply $ok, [<x y>], 'and-RHS assignment yields the assigned value';
}

# Self-assignment keeps working.
{
    my @a = 1, 2;
    True and @a = @a;
    is-deeply @a, [1, 2], 'self-assignment keeps contents';
}

# Copy applies element-wise snapshot, not a lazy alias, in ternary position too.
{
    my @src = 4, 5;
    my @t;
    my $r = 1 ?? (@t = @src) !! Nil;
    @src[0] = 99;
    is-deeply @t, [4, 5], 'ternary-position array assignment copies';
}
