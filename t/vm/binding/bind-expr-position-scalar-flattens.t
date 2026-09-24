use Test;

# A scalar `:=` bind written in EXPRESSION position (`(my $p := EXPR)`)
# aliases the value just like the statement form, so `@a = $p` flattens a
# Positional rather than nesting it as one item (#9262).

plan 7;

{
    (my $p := (1, 2));
    my @w = $p;
    is @w.raku, '[1, 2]', 'mainline expression-position bind flattens on @-assign';
}

{
    my $x = (my $q := (1, 2));
    my @z = $q;
    is @z.raku, '[1, 2]', 'bind used as an assignment RHS still flattens';
    is $x.raku, '$(1, 2)', 'the assigned-to scalar itself is itemized';
}

{
    my @y;
    if (my $s := (1, 2)) { @y = $s }
    is @y.raku, '[1, 2]', 'bind in an if condition flattens';
}

{
    my $it = (1 .. 3).rotor(1).iterator;
    my &f = { (my $p := $it.pull-one) =:= IterationEnd ?? () !! $p };
    my @out;
    while f() -> @vals { @out.append(@vals) }
    is @out.raku, '[1, 2, 3]', 'one-element chunks returned through a closure flatten';
}

{
    sub natatime($n, @values) {
        my $iterator := @values.rotor($n, :partial).iterator;
        return {
            (my $pulled := $iterator.pull-one) =:= IterationEnd ?? () !! $pulled
        }
    }
    my @a = 1 .. 3;
    my $it = natatime 1, @a;
    my @out;
    while $it() -> @vals { @out.append(@vals) }
    is @out.raku, '[1, 2, 3]', 'List::MoreUtils-style natatime 1';

    my $it3 = natatime 2, @a;
    my @chunks;
    while $it3() -> @vals { @chunks.push(@vals.join(',')) }
    is @chunks.raku, '["1,2", "3"]', 'natatime 2 still chunks';
}
