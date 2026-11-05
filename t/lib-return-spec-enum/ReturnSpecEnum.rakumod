unit module ReturnSpecEnum;

enum Precedence <Rowlike Comma Termlike>;

role Term { method precedence(--> Precedence::Termlike) {} }
class Leaf does Term is export { }

sub qualified(--> Precedence::Comma) is export { }
sub bare(--> Comma) is export { }
sub fully(--> ReturnSpecEnum::Precedence::Rowlike) is export { }
