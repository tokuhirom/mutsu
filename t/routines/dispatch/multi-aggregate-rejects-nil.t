use v6;
use Test;

# P5defined's `defined($scalar)` reaches its scalar multi candidate after
# `undef($scalar)` stores Nil. Nil is not a Positional or Associative value, so
# an aggregate-sigil candidate must not claim it during multi dispatch.

plan 2;

multi sub array-choice(@values) { 'array' }
multi sub array-choice(\value) { 'item' }

multi sub hash-choice(%values) { 'hash' }
multi sub hash-choice(\value) { 'item' }

is array-choice(Nil), 'item', '@ candidate does not match Nil';
is hash-choice(Nil), 'item', '% candidate does not match Nil';
