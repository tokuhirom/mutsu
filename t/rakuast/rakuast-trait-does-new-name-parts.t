use Test;

plan 7;

my $type = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("S"));
my $does = RakuAST::Trait::Does.new($type);
is $does.^name, 'RakuAST::Trait::Does', 'Trait::Does.new(TYPE) is a known constructor';
is $does.type.^name, 'RakuAST::Type::Simple', 'its .type is the positional type';

my $name = RakuAST::Name.from-identifier("a");
is $name.parts.elems, 1, 'from-identifier name has one part';
is $name.parts[0].^name, 'RakuAST::Name::Part::Simple', 'the part is a Name::Part::Simple';
is $name.parts[0].name, 'a', 'the part carries the identifier';
is RakuAST::Name.from-identifier("A::B").parts.elems, 1, 'a from-identifier spelling stays one part';
is $name.canonicalize, 'a', 'canonicalize answers the spelling';
