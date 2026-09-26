use Test;

plan 5;

# A statement modifier whose operand ends in `}` followed by a newline ends
# the statement only when that `}` closes a *block*. A hash subscript's `}`
# is an ordinary term end, so the next line's modifier still belongs to the
# same statement. App::Lorea writes
#
#     die "--$_ is not allowed in config" if %hash{$_}
#         for < config help >;
#
# which failed to parse ("Missing block": the `for` was taken as a new
# statement with no block).

{
    my %h;
    my @seen;
    @seen.push($_) if %h{$_}
        for <a b>;
    is-deeply @seen, [], 'if COND-ending-in-subscript, newline, for LIST';
}

{
    my %h = b => 1;
    my @seen;
    @seen.push($_) unless !%h{$_}
        for <a b>;
    is-deeply @seen, ['b'], 'unless with a prefix-negated subscript operand';
}

{
    my %h = a => 1;
    my @seen;
    @seen.push($_) if True && %h{$_}
        for <a b>;
    is-deeply @seen, ['a'], 'subscript as the right operand of an infix';
}

{
    my @a = 0, 1;
    my @seen;
    @seen.push($_) if @a[$_]
        for 0, 1;
    is-deeply @seen, [1], 'array subscript (ends in `]`) keeps working';
}

{
    # A condition that really ends in a block still terminates the statement.
    my @a = 1, 2;
    my $r = 0;
    $r = 1 if @a.first: { $_ > 1 }
    $r += 10;
    is $r, 11, 'a block-final modifier operand still ends the statement';
}
