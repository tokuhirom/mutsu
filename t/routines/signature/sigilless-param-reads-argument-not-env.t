use v6;
use lib 't/lib';
use Test;
use SigillessParamAcrossModule;

plan 14;

# A sigilless parameter (`\c`) is implicitly raw: it aliases the caller's
# container. mutsu recorded that alias by *name* and then re-read the caller's
# variable out of `env` to produce the bound value. When the live value lived
# in the caller's local slot and `env` still held the declaration-time one --
# which is what a cross-compunit method call leaves behind -- the parameter
# bound the variable's TYPE OBJECT instead of its value:
#
#   my Positional $t = <foo bar>;
#   Mod::C.m($t);     # `\c` inside `m` saw `(Positional)`, and the exit
#                     # writeback then stamped that back onto `$t`
#
# The alias is what the re-read is for; the VALUE must come from the argument
# the VM evaluated at the callsite. Explicit `is raw` / `is rw` parameters
# never re-read, which is why only the sigilless spelling was affected.

my $C = SigillessParamAcrossModule;

{
    my Positional $t = <foo bar>;
    is-deeply $C.peek($t), $($t), 'sigilless param sees a Positional-typed scalar';
    is-deeply $t, $(<foo bar>), 'and the caller variable is unchanged';
}

{
    my Associative $h = {:a(1)};
    is-deeply $C.peek($h), $({:a(1)}), 'sigilless param sees an Associative-typed scalar';
    is-deeply $h, $({:a(1)}), 'and the caller variable is unchanged';
}

{
    my Int $n = 7;
    is $C.peek($n), 7, 'sigilless param sees an Int-typed scalar';
    is $n, 7, 'and the caller variable is unchanged';
}

{
    my $u = <a b>;
    is-deeply $C.peek($u), $(<a b>), 'sigilless param sees an untyped scalar';
    is-deeply $u, $(<a b>), 'and the caller variable is unchanged';
}

{
    my @a = <x y>;
    is-deeply $C.peek(@a), <x y>.Array, 'sigilless param sees an array variable';
    is-deeply @a, <x y>.Array, 'and the caller array is unchanged';
}

{
    my Int $n = 3;
    is-deeply $C.two($n, 9), (3, 9), 'sigilless param alongside an ordinary one';
    is $n, 3, 'and the caller variable is unchanged';
}

# An exported sub with the same signature shape never had the bug; pin it so a
# future change keeps the two spellings in agreement.
{
    my Positional $t = <foo bar>;
    is-deeply peek-sub($t), $($t), 'sigilless param of a module sub sees the argument';
    is-deeply $t, $(<foo bar>), 'and the caller variable is unchanged';
}
