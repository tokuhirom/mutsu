use v6;
use experimental :rakuast;
use Test;

# A Perl 5 dereference block inside an interpolating string is a compile-time
# X::Obsolete, for the source text and for its `.AST` alike.

plan 4;

throws-like { EVAL Q["${$scalar}"] }, X::Obsolete, 'EVAL of "${$scalar}" is obsolete';
throws-like { EVAL Q["@{@array}"] }, X::Obsolete, 'EVAL of "@{@array}" is obsolete';
throws-like { Q["${$scalar}"].AST }, X::Obsolete, '.AST of "${$scalar}" is obsolete';
lives-ok { EVAL Q["$(1 + 1)"].AST }, 'an ordinary interpolation round trips';
