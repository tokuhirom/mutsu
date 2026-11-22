use Test;

# `CALLER::LEXICAL::` lists the routines of the caller's lexical scope: its
# file-scope subs, the subs of its package, and its own `my sub`s (#17733).

plan 6;

sub lookup(Str $x) { with CALLER::LEXICAL::.AT-KEY('&' ~ $x) { .name } else { 'none' } }
sub plain($x) { $x }

is lookup('plain'), 'plain', 'a file-scope sub of the caller';
is lookup('no-such-routine'), 'none', 'a name the caller does not see';

sub names { CALLER::LEXICAL::.keys.grep(* eq '&plain' | '&names' | '&lookup').sort.List }
is-deeply names(), ('&lookup', '&names', '&plain'), 'the stash lists the caller file-scope subs';

module Pkg {
    our sub ask { lookup('helper') }
    sub helper { 1 }
}
is Pkg::ask(), 'helper', 'a sub of the calling routine package';

sub with-inner {
    my sub inner-only { 1 }
    lookup('inner-only')
}
is with-inner(), 'inner-only', 'a `my sub` of the calling routine';

is CALLER::LEXICAL::.AT-KEY('&plain').defined, False,
    'the mainline has no caller pad of its own';
