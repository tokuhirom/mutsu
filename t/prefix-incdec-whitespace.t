use Test;

plan 7;

{
    my $count = 0;
    ++ $count;
    is $count, 1, 'prefix ++ accepts optional whitespace before its operand';
}

{
    my $count = 5;
    -- $count;
    is $count, 4, 'prefix -- accepts optional whitespace before its operand';
}

{
    # Minimized from Archive::SimpleZip's `add` method
    # ($s.map: { samewith($^a, |c) ; ++ $count} ;) -- the ++ there is
    # preceded by a space and followed by a `;`-terminated statement list
    # inside a colon-call block argument.
    my $s = [1, 2, 3];
    my $count = 0;
    $s.map: { ++ $count };
    is $count, 3, 'prefix ++ with whitespace parses inside a .map: block';
}

{
    my $count = 0;
    (1, 2, 3).map: { my $ignore = 1; ++ $count };
    is $count, 3, 'prefix ++ with whitespace parses after a sequenced statement';
}

{
    # ++ still binds tighter than ** regardless of the intervening whitespace.
    my $i = 1;
    is (++ $i ** 2), 4, 'whitespace before the operand does not change ++ vs ** precedence';
}

{
    my $i = 5;
    is (-- $i * 2), 8, 'whitespace before the operand does not change -- vs * precedence';
}

# Chaining two prefix ++ is still non-associative and must error, whitespace
# or not.
dies-ok { EVAL 'my $y = 1; ++ ++ $y;' },
    'chained prefix ++ with whitespace is still rejected as non-associative';
