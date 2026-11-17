unit module PseudoStashUnitReader;

# A computed name with a constant package prefix names a package member, never
# a lexical (#17998).
our $answer = 42;
class Thing { }
sub package-member($name) is export { ::("PseudoStashUnitReader::$name") }
