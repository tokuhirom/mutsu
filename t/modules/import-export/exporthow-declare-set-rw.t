use Test;

# #13624 (ValueClass 0.0.10): `is rw` on a class declared with an
# EXPORTHOW::DECLARE declarator reaches the HOW's `set_rw`.
use lib 't/lib';
use DeclareSetRwHow;

plan 3;

lives-ok { EVAL 'use DeclareSetRwHow; my frozen Plain { has $.a }' }, 'no `is rw`: set_rw is not called';

my $worked = try { EVAL 'use DeclareSetRwHow; my frozen Loose is rw { }'; True };
nok $worked, '`is rw` on the declaration reaches set_rw, which dies';
ok $!.message.contains("can't be rw"), 'with the HOW message';
