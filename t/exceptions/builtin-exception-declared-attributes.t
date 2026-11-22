use Test;

# The built-in exception classes declare rakudo's attributes (#18654), so
# `.^attributes` lists them, construction seeds every declared slot (with its
# literal initializer, else the type object an unset one holds), and every
# attribute has its reader. The message formatters read an unset slot as
# absent, as rakudo's test `.defined`.

plan 21;

is X::NYI.^attributes.map(*.name).grep(none('$!ex', '$!bt')).join(' '),
    '$!feature $!did-you-mean $!workaround', 'X::NYI declares its attributes';
is X::OutOfRange.^attributes.map(*.name).grep(none('$!ex', '$!bt')).join(' '),
    '$!what $!got $!range $!comment', 'X::OutOfRange declares its attributes';
ok X::Comp::AdHoc.^attributes.elems > 0, 'a class composing X::Comp lists its role attributes';

is X::NYI.new(feature => 'x').did-you-mean.raku, 'Any', 'an unset attribute reads as Any';
is X::Method::NotFound.new(method => 'm', typename => 'T').private.raku, 'Bool',
    'an unset typed attribute reads as its type object';
is X::Method::NotFound.new(method => 'm', typename => 'T').invocant.raku, 'Mu',
    'an unset Mu attribute reads as Mu';

# Literal initializers.
is X::OutOfRange.new.what, 'Argument', 'a Str initializer';
is X::OutOfRange.new.got, '<unknown>', 'another Str initializer';
is X::Redeclaration.new(symbol => 'foo').what, 'symbol', 'X::Redeclaration.what defaults';
is X::Undeclared.new(symbol => 'x').is-compile-time, False, 'a Bool initializer';
is X::AdHoc.new.payload, 'Unexplained error', 'X::AdHoc.payload defaults';

# The formatters read an unset slot as absent.
is X::OutOfRange.new(what => 'Index', got => 5, range => '0..3').message,
    'Index out of range. Is: 5, should be in 0..3', 'no comment, no suffix';
is X::OutOfRange.new(what => 'Index', got => 5, range => '0..3', comment => 'c').message,
    'Index out of range. Is: 5, should be in 0..3; c', 'a comment is appended';
is X::AdHoc.new.message, 'Unexplained error', 'X::AdHoc with no payload';
is X::StubCode.new.message, 'Stub code executed', 'X::StubCode.message defaults';
is X::Redeclaration.new(symbol => 'foo').message, "Redeclaration of symbol 'foo'.",
    'X::Redeclaration message';
is X::NYI.new(feature => 'x').message, 'x not yet implemented. Sorry.', 'X::NYI message';

# Rakudo's explicit reader methods for private `is built` attributes.
my $tc = X::TypeCheck.new(operation => 'op', got => 1, expected => Str);
is $tc.got, 1, 'X::TypeCheck.got';
is $tc.expected.raku, 'Str', 'X::TypeCheck.expected';

is X::NYI.new(feature => 'x').raku,
    'X::NYI.new(feature => "x", did-you-mean => Any, workaround => Any)',
    '.raku lists every declared attribute';

class MyErr is X::OutOfRange { }
is MyErr.new(got => 3).what, 'Argument', 'a subclass inherits the attributes and initializers';
