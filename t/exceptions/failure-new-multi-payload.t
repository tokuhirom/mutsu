use Test;

plan 8;

# `Failure.new` with several payloads builds its X::AdHoc through
# `X::AdHoc.from-slurpy`, concatenating every payload (#18984;
# raku-doc Type/Failure.rakudoc, "method new").
my $f = Failure.new("a", "b");
$f.so;
is $f.exception.message, 'ab', 'two payloads concatenate into the message';
is $f.exception.^name, 'X::AdHoc', 'the exception is an X::AdHoc';
is $f.exception.payload.^name, 'Capture+{X::AdHoc::SlurpySentry}',
    'the payload is the SlurpySentry capture';

my $d = Failure.new(Date.new(2017, 9, 10), 'WELP‼');
$d.so;
is $d.exception.message, '2017-09-10WELP‼', 'a non-string payload is stringified';

# An exception among several payloads is stringified too.
my $e = Failure.new(X::AdHoc.new(payload => 'z'), 'q');
$e.so;
is $e.exception.message, 'zq', 'an exception payload contributes its message';

# A single payload keeps its own value.
my $one = Failure.new('a');
$one.so;
is-deeply $one.exception.payload, 'a', 'a single payload is kept as is';

# `X::AdHoc.message` is `payload.Str`.
is X::AdHoc.new(payload => ('a', 'b')).message, 'a b',
    'a list payload renders its elements space-joined';
is X::AdHoc.new(payload => 42).message.^name, 'Str', 'the message is a Str';
