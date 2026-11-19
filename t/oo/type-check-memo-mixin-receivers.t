use Test;

# Type checks whose answers the per-(class, type) memo now keeps for more
# receivers: a meta-object with a role mixed in (`.HOW does R`), an object
# with a role mixed in, a Capture, and a built-in scalar against a lexical
# (`my class`) type. Each is asked twice, so a wrong memoized answer shows on
# the repeat. Reduced from AttrX::Mooish (#17335).

plan 20;

role HowRole { method extra { 1 } }
class Plain { has $.x }
my $how = Plain.HOW;
$how does HowRole;

for ^2 {
    nok $how ~~ Metamodel::GenericHOW, "a mixed-in ClassHOW is not a GenericHOW (round $_)";
    ok  $how ~~ Metamodel::ClassHOW,   "a mixed-in ClassHOW is a ClassHOW (round $_)";
    ok  $how ~~ HowRole,               "a mixed-in ClassHOW does its role (round $_)";
}

role Marker { method marked { True } }
my $attr = Plain.^attributes[0];
$attr does Marker;
class Other { has $.y }
my $other = Other.^attributes[0];
for ^2 {
    ok  $attr ~~ Marker,    "an Attribute with a role mixed in does it (round $_)";
    ok  $attr ~~ Attribute, "an Attribute with a role mixed in is an Attribute (round $_)";
    nok $other ~~ Marker,   "another Attribute without the role does not (round $_)";
}

sub takes-any($x) { 'any' }
is takes-any($attr), 'any', 'a mixed-in Attribute binds an untyped parameter';

my class Lexical { }
for ^2 {
    nok 42 ~~ Lexical,      "an Int is not a lexical class (round $_)";
    ok  Lexical.new ~~ Lexical, "an instance is its lexical class (round $_)";
}

sub cap(Capture:D $c) { $c.elems }
is cap(\(1, 2, 3)), 3, 'a Capture binds a Capture:D parameter';
is cap(\()), 0, 'an empty Capture binds a Capture:D parameter';
nok \(1) ~~ Positional:U, 'a Capture is not an undefined Positional';
