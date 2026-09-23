use Test;

plan 3;

# Regression from Pod::Utils 0.0.2: parser-created Pod objects have bare
# attributes, while constructor-created objects also carry equal inherited
# attribute mirrors under qualified storage keys.
my $parsed = $=pod[0].contents[0];
my $constructed = Pod::Block::Named.new(
    name     => "TITLE",
    contents => [Pod::Block::Para.new(contents => ["title"])]
);
is-deeply $constructed, $parsed,
    "constructor and parser Pod objects compare deeply equal";

class Parent {
    has $.value;
    method set-parent($value) { $!value = $value }
}

class Child is Parent {
    has $.value;
}

my $same-slots = Child.new(value => 1);
my $same-slots-copy = Child.new(value => 1);
is-deeply $same-slots, $same-slots-copy,
    "equal qualified attribute slots remain eqv";

$same-slots.set-parent(2);
nok $same-slots eqv $same-slots-copy,
    "different qualified attribute slots are not eqv";

=begin pod

=TITLE title

=end pod

done-testing;
