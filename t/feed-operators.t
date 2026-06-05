use Test;

plan 16;

{
    my @a = (1, 2);
    my (@b, @c);
    @a ==> @b;
    @c <== @a;
    is(~@b, ~@a, '==> assigns into array sink');
    is(~@c, ~@a, '<== assigns from right-hand source');
}

{
    my @a = (1 .. 5);
    my @b;
    @a ==> grep { ($_ % 2) == 0 } ==> @b;
    is(~@b, '2 4', 'feed chain through callable works');
}

{
    my @tap;
    my @result = do { @tap <== grep { $_ % 2 } <== eager (1 .. 6) };
    is(~@tap, '1 3 5', 'tap sink receives intermediate feed values');
    is(~@result, '1 3 5', 'tap sink preserves downstream value');
}

dies-ok { my @a <== 0..Inf }, 'feeding an infinite range into an array dies';

# Feed into an inline declaration `... ==> my @o` followed by another statement
# on the same line. The terminal declaration must not swallow the `;` and the
# following statement (regression: `my @o; say @o` parsed as `say` infix on the
# feed, so @o stayed empty and the result printed twice).
{
    (1, 2, 3) ==> map({ $_ * 2 }) ==> my @o;
    is(~@o, '2 4 6', 'feed into inline `my @o` declaration');
}
{
    (1, 2, 3, 4) ==> grep({ $_ %% 2 }) ==> my @evens;
    is(~@evens, '2 4', 'feed into inline `my @evens` with following stmt');
}
{
    my $marker = 0;
    (1, 2, 3) ==> my @xs; $marker = 42;
    is(~@xs, '1 2 3', 'inline feed decl does not swallow the next statement');
    is($marker, 42, 'statement after inline feed decl still runs');
}
{
    # Chained feed terminating in a declaration, then an independent statement.
    (5, 1, 3) ==> sort() ==> my @sorted;
    my @other = 7, 8;
    is(~@sorted, '1 3 5', 'chained feed into inline decl');
    is(~@other, '7 8', 'declaration after chained feed is independent');
}

# Feeding into a scalar materializes the feed as an Array (matches Raku):
# `(1,2,3) ==> $x` leaves `$x` as `[1 2 3]`, not a bare List.
{
    (1, 2, 3) ==> map({ $_ + 10 }) ==> my $x;
    is($x.^name, 'Array', 'feed into inline `my $x` produces an Array');
    is(~$x, '11 12 13', 'feed into inline `my $x` holds the materialized feed');
}
{
    my $y;
    (4, 5) ==> $y;
    is($y.^name, 'Array', 'feed into pre-declared scalar produces an Array');
}
{
    42 ==> my $z;
    is(~$z, '42', 'feed of a single value into a scalar is a 1-element Array');
}
