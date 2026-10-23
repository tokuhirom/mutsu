use Test;

# #14588 (ValueClass 0.0.10): the `<declarator>-attr` entry of EXPORTHOW::DECLARE
# is the metaclass of the `has` attributes of a class declared with
# `<declarator>`; its `compose` runs when the class is composed.
use lib 't/lib';
use DeclareAttrMetaHow;

plan 8;

lives-ok { EVAL 'use DeclareAttrMetaHow; my frozenattr Plain { has $.a; has $!b }' },
    'attributes that pass the metaclass compose';

my $worked = try { EVAL 'use DeclareAttrMetaHow; my frozenattr Loose { has $.a is rw }'; True };
nok $worked, 'an `is rw` attribute dies in the attribute metaclass compose';
ok $!.message.contains("can't be rw on a frozenattr class"), 'with the metaclass message';

lives-ok { EVAL 'my class Plainer { has $.a is rw }' }, 'an ordinary class is unaffected';

my \Holder = EVAL 'use DeclareAttrMetaHow; my frozenattr :: { has @.items; has $.single }';
is Holder.new.items.^name, 'List', '`container_initializer` supplies the container of an @ attribute';
is Holder.new.single.^name, 'Any', 'a scalar attribute keeps its default';

is Holder.^attributes.map(*.^name).unique.join(','), 'DeclareAttrMeta',
    '`.^attributes` answers instances of the declarator\'s -attr class';
is Holder.^attributes.map(*.name).sort.join(','), '$!single,@!items', 'with the attributes\' own names';
