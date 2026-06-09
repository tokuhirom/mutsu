use Test;

# Placeholder/twigil variables are not allowed as signature parameters.
#   $:x @:x %:x  -> X::Parameter::Placeholder (named)
#   $^x          -> X::Parameter::Placeholder (positional)
#   $?x $=x $~x  -> X::Parameter::Twigil

plan 11;

# Named placeholder: X::Parameter::Placeholder, right => ':$x'
throws-like 'sub f($:x) { }', X::Parameter::Placeholder,
        parameter => '$:x', right => ':$x';
throws-like 'sub f(@:x) { }', X::Parameter::Placeholder,
        parameter => '@:x', right => ':@x';
throws-like 'sub f(%:x) { }', X::Parameter::Placeholder,
        parameter => '%:x', right => ':%x';

# Positional placeholder: X::Parameter::Placeholder, right => '$x'
throws-like 'sub f($^x) { }', X::Parameter::Placeholder,
        parameter => '$^x', right => '$x';

# Twigil parameters: X::Parameter::Twigil
throws-like 'sub f($?x) { }', X::Parameter::Twigil,
        parameter => '$?x', twigil => '?';
throws-like 'sub f($=x) { }', X::Parameter::Twigil,
        parameter => '$=x', twigil => '=';
throws-like 'sub f($~x) { }', X::Parameter::Twigil,
        parameter => '$~x', twigil => '~';

# Legal forms must still parse and run.
ok (sub ($*x) { $*x })(7) == 7, '$*x dynamic var param is legal';
ok (sub (:$x) { $x })(x => 3) == 3, ':$x named param is legal';
ok (sub (:$x!) { $x })(x => 4) == 4, ':$x! required named param is legal';
ok { $^a + $^b }(2, 3) == 5, 'placeholders in a block body are legal';
