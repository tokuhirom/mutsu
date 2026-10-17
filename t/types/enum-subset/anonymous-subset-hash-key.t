use Test;
use experimental :rakuast;

plan 11;

sub key-type($source) {
    $source.AST.statements[0].expression.body.body.statement-list.statements[0]
        .expression.shape.statements.head.expression
}

my $type = key-type(Q[class Keyed1 { has Str %.bla{subset :: of Str where any("ble", "blob")}; }]);
is $type.^name, 'RakuAST::Type::Subset', 'the hash key remains a Type::Subset';
is $type.name.parts.elems, 2, 'the anonymous subset has both measured name parts';
is $type.name.parts[0].^name, 'RakuAST::Name::Part::Empty', 'the name starts with Rakudo’s empty part';
is $type.name.parts[1].^name, 'RakuAST::Name::Part::Empty', 'the anonymous subset keeps both empty name parts';
is $type.traits[0].^name, 'RakuAST::Trait::Of', 'the explicit base remains a Trait::Of';
is $type.where.^name, 'RakuAST::Call::Name', 'the subset predicate stays structured';

is EVAL(Q[
    class Keyed2 { has Str %.bla{subset :: of Str where any("ble", "blob")}; }
    my $object = Keyed2.new;
    $object.bla<ble> = 'bli';
    $object.bla<ble>
].AST), 'bli', 'a key accepted by the inline subset round-trips';
throws-like {
    EVAL(Q[
        class Keyed3 { has Str %.bla{subset :: of Str where any("ble", "blob")}; }
        my $object = Keyed3.new;
        $object.bla<nope> = 'wrong';
    ].AST)
}, Exception, 'an invalid inline-subset key is rejected after lowering';
is EVAL(Q[
    class Keyed4 { has Str %.bla{subset :: of Str where any("ble", "blob")}; }
    my $object = Keyed4.new;
    $object.bla<ble> = 'bli';
    $object.bla.elems
].AST), 1, 'the rejected key does not add an object-hash entry';

ok EVAL(Q[
    class KH1 { has Str %.bla{subset :: of Str where any("ble", "blob")} }
    KH1.^attributes[0].type.HOW.archetypes.nominalizable.Bool
].AST) === False, 'an inline subset hash key preserves its value-type archetype';
is EVAL(Q[
    class KH2 { has Str %.bla{subset :: of Str where any("ble", "blob")} }
    KH2.^attributes[0].type.of.^name
].AST), 'Str', 'the object-hash value type remains Str';
