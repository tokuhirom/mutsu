use Test;

plan 6;

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
