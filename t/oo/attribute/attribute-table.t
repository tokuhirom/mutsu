use Test;

class AttributeTableParent {
    has $.inherited;
}

class AttributeTableFixture is AttributeTableParent {
    has $.visible;
    has $!private;
}

plan 3;

my %table = AttributeTableFixture.^attribute_table;
is %table.keys.sort.join(','), '$!private,$!visible',
    'ClassHOW.^attribute_table returns local attributes by full name';
is %table{'$!visible'}.name, '$!visible',
    'attribute_table values are Attribute objects';
is %table{'$!private'}.type.^name, 'Mu',
    'attribute_table preserves the attribute metadata';
