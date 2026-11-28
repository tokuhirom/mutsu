use Test;

# Rakudo 2026.11 declares `properties` and `reducer-name` on
# RakuAST::MetaInfix::CrossZip, the shared parent of `Cross` and `Zip`,
# instead of on each of them (#19274).

plan 7;

my $cross = RakuAST::MetaInfix::Cross.new(RakuAST::Infix.new("+"));
is $cross.reducer-name, '&METAOP_REDUCE_LISTINFIX', 'Cross inherits reducer-name';
is $cross.properties.^name, 'OperatorProperties', 'Cross inherits properties';

my $zip = RakuAST::MetaInfix::Zip.new(RakuAST::Infix.new("~"));
is $zip.reducer-name, '&METAOP_REDUCE_LISTINFIX', 'Zip inherits reducer-name';

is-deeply RakuAST::MetaInfix::CrossZip.^methods(:local).map(*.name).grep(/reducer|propert/).sort.List,
    <properties reducer-name>, 'CrossZip declares both';
is-deeply RakuAST::MetaInfix::Cross.^methods(:local).map(*.name).grep(/reducer|propert/).List,
    (), 'Cross does not declare them itself';
is-deeply RakuAST::MetaInfix::Zip.^methods(:local).map(*.name).grep(/reducer|propert/).List,
    (), 'nor does Zip';
ok RakuAST::MetaInfix::Cross.^can('reducer-name'), '.^can finds the inherited method';
