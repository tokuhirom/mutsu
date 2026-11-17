use Test;

# A constant return spec (`--> 0`, `--> True`, `--> Bool::True`) is the
# routine's value, not a type: the signature prints it, `.returns` answers
# the value's type, and a setting candidate keeps its own (#18010). Checked
# against rakudo.

plan 10;

sub zero(--> 0) { }
is &zero.signature.raku, ':( --> 0)', 'a constant return prints in the signature';
is &zero.returns.^name, 'Int', '... and .returns is its type';

sub yes(--> True) { }
is &yes.returns.^name, 'Bool', '--> True returns Bool';

sub nothing(--> Nil) { }
is &nothing.returns.raku, 'Nil', '--> Nil returns Nil';

sub typed(--> Int) { }
is &typed.returns.^name, 'Int', 'a type return is unchanged';

sub qualified(--> Bool::True) { }
is qualified(), True, 'a qualified core enum value is a constant return';
is &qualified.returns.^name, 'Bool', '... whose .returns is its enum';

sub less(--> Order::Less) { }
is less(), Less, 'an Order value too';

is &infix:<+>.candidates[0].signature.raku, ':( --> 0)',
    "a setting candidate's constant return prints";
is &infix:<==>.candidates[0].returns.^name, 'Bool',
    "... and its .returns is the value's type";
