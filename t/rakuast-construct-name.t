use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 2 (ADR-0011): `RakuAST::Name.from-identifier("x")`.
# The `.from-identifier` constructor builds a Name node whose gist round-trips.
# Verified against Rakudo; this file passes under BOTH mutsu and raku.

plan 3;

is RakuAST::Name.from-identifier("x").gist, 'RakuAST::Name.from-identifier("x")',
    'Name.from-identifier gist round-trips';

is RakuAST::Name.from-identifier("foo").^name, 'RakuAST::Name',
    'the constructed node is a RakuAST::Name';

# --- a constructed Name can be used as the name of a constructed call --------
# (round-trips through gist as a Name inside its container is future work; here
# we just confirm the node is a first-class RakuAST value.)
ok RakuAST::Name.from-identifier("x") ~~ RakuAST::Node,
    'a constructed Name isa RakuAST::Node';
