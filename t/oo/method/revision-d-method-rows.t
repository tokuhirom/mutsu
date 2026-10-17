use v6.d;
use Test;

# ADR-11276 §9.146: the methods language revision 6.e adds are absent under
# 6.d: the call finds no method and `.^can` denies it, as in Rakudo
# (`revision-e-method-rows.t` has them under 6.e).

plan 8;

throws-like { (1, 2).snip(* < 2) }, X::Method::NotFound, 'no Any.snip';
throws-like { (1, 2).snitch }, X::Method::NotFound, 'no Any.snitch';
throws-like { "é".nomark }, X::Method::NotFound, 'no Str.nomark';
throws-like { "a.b".IO.stem }, X::Method::NotFound, 'no IO::Path.stem';

nok Any.^can('snip'), 'Any.^can("snip") is false';
nok Any.^can('snitch'), 'Any.^can("snitch") is false';
nok Str.^can('nomark'), 'Str.^can("nomark") is false';
nok IO::Path.^can('stem'), 'IO::Path.^can("stem") is false';
