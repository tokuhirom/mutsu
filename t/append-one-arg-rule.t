use Test;

plan 8;

# Single array arg: flattened
{
    my @a;
    @a.append([1, 2, 3]);
    is @a.elems, 3, 'append with single array arg flattens';
}

# Single list arg via parens: parens are grouping, so elements spread
{
    my @a;
    @a.append((1, 2, 3));
    is @a.elems, 3, 'append with single list arg flattens';
}

# Multiple args: no flattening
{
    my @a;
    @a.append("fmt", (1, 2, 3));
    is @a.elems, 2, 'append with multiple args does not flatten';
    is @a[1].elems, 3, 'second element is preserved as list';
}

# Multiple args with pairs
{
    my @a;
    @a.append("fmt", ("a" => 1, "b" => 2, "c" => 3));
    is @a.elems, 2, 'append with str + pair-list: 2 elements';
    is @a[1].elems, 3, 'pair-list preserved as single element with 3 pairs';
}

# Itemized single arg: not flattened
{
    my @a;
    @a.append($[1, 2, 3]);
    is @a.elems, 1, 'append with itemized array: not flattened';
}

# Multiple calls to append
{
    my @a;
    @a.append("x", (1, 2));
    @a.append("y", (3, 4));
    is @a.elems, 4, 'multiple append calls accumulate correctly';
}
