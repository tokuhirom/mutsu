use v6;
use Test;

# `.reduce(&[~])` with the builtin `~` folds through the `~` operator itself,
# moving the accumulator into each step so an unshared accumulated Str grows
# in place (#9161; the routine-call path held a second reference and copied
# the whole string every step). The fold must keep every rule of infix `~`.

plan 11;

is (1, 2, 3).reduce(&[~]), '123', 'Int elements stringify';
is (1, 2, 3).reduce(&[~]).WHAT, Str, 'the result is a Str';
is (1..5).reduce(&infix:<~>), '12345', '&infix:<~> spelling';

{
    my class S { method Str { "S!" } }
    is (S.new, "x", S.new).reduce(&[~]), 'S!xS!', 'a user .Str is honoured';
}

is (Blob.new(1, 2), Blob.new(3)).reduce(&[~]), Blob.new(1, 2, 3), 'Blob elements concatenate as bytes';
is ("e", "\x[301]").reduce(&[~]).chars, 1, 'a combining mark composes across the join';
is ().reduce(&[~]).raku, '""', 'empty list is the identity';
is ("z",).reduce(&[~]), 'z', 'one element';

{
    my @a = ^20000;
    my $s = @a.reduce(&[~]);
    is $s.chars, 88890, 'a long fold keeps every character';
    is $s.substr(0, 5), '01234', 'and starts with the first elements';
    is $s.substr(*-5), '19999', 'and ends with the last';
}
