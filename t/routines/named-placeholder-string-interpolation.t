use v6;
use Test;

plan 2;

sub bare-named-placeholder { $:foo }
sub string-named-placeholder { "$:foo" }

is &bare-named-placeholder.assuming(foo => 42)(), 42,
    'named placeholder works as a bare variable';
is &string-named-placeholder.assuming(foo => 42)(), '42',
    'named placeholder interpolates inside a double-quoted string';
