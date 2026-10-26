use v6;
use Test;

# An empty statement (`;;`) is no statement of the lowered tree: the value of
# a routine or block is the statement before it, in the executing round trip
# (MUTSU_RAKUAST=1) as in the ordinary frontend.
plan 5;

sub one-semi { 1; }
sub two-semis { 1;; }
sub three-semis { 1;;; }

is one-semi(), 1, 'one terminator';
is two-semis(), 1, 'a stray ; after the last statement keeps its value';
is three-semis(), 1, 'several stray ; keep it too';
is { 2;; }(), 2, 'in a bare block';
is (do { 3;; }), 3, 'in a do block';
