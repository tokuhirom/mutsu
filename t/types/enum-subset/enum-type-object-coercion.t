use Test;

# Protocol::Postgres decodes enum fields by calling a lexical enum type object.
plan 2;

enum ProtocolTestRequestType (:Prepared(83), :Portal(80));
my $type = ProtocolTestRequestType;

is $type(83).^name, 'ProtocolTestRequestType', 'a lexical enum type object coerces its value';
is $type(83).key, 'Prepared', 'enum type-object coercion selects the matching variant';
