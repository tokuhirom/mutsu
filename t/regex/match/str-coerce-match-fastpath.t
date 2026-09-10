use v6;
use Test;

# Pins the `~Match` Str-coercion fast path (exec_str_coerce_op): a plain
# Match stringifies to its matched text, and a user-defined prefix:<~>
# overload must still be consulted BEFORE the fast path reads the `str`
# attribute directly.

plan 5;

my $m = "abcdef" ~~ /b.d/;
is ~$m, "bcd", 'prefix:<~> on a Match returns the matched string';
isa-ok ~$m, Str, '~Match yields a Str';
is "$m", "bcd", 'string interpolation of a Match';
is ~($m<>), "bcd", '~ on a decontainerized Match';

{
    multi sub prefix:<~>(Match $x) { "overloaded" }
    my $n = "xyz" ~~ /y/;
    is ~$n, "overloaded", 'a user prefix:<~> overload beats the Match fast path';
}
