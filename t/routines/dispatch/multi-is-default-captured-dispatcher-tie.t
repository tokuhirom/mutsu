use v6;
use lib 't/lib';
use Test;
use IsDefaultMultiModule;

plan 2;

# A module's `my proto` + `my multi` family is called through the captured
# dispatcher. `is default` must still break the tie between `(Str:D $t)` and
# `(Str:D $t, *%_)` (#15132, Text::Emoji's `to-emoji`).
is to-emoji("x"), 'one', 'the is default candidate wins the tie';
is to-emoji("x", :a), 'two', 'the slurpy-hash candidate re-dispatches';
