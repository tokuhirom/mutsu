use Test;

# #18645: `.raku` of a built-in exception lists every attribute its class
# declares (Rakudo's `Mu.raku`), unset ones as their type object; and `.raku`
# of any object lists a private attribute declared `is built`.

plan 7;

is X::NYI.new(feature => "x").raku,
    'X::NYI.new(feature => "x", did-you-mean => Any, workaround => Any)',
    'X::NYI with unset attributes';
is X::NYI.new(feature => "x", workaround => "w").raku,
    'X::NYI.new(feature => "x", did-you-mean => Any, workaround => "w")',
    'X::NYI with a set attribute';
is X::OutOfRange.new(what => "w", got => 5, range => 1..3).raku,
    'X::OutOfRange.new(what => "w", got => 5, range => 1..3, comment => Any)',
    'X::OutOfRange';
is X::Method::NotFound.new(method => "m", typename => "T").raku,
    'X::Method::NotFound.new(invocant => Mu, method => "m", typename => "T", private => Bool, addendum => Any, in-class-call => Any, containerized => Any)',
    'a typed attribute renders as its type object';
is X::TypeCheck.new(operation => "op", got => 1, expected => Str).raku,
    'X::TypeCheck.new(operation => "op", got => 1, expected => Str)',
    'private attributes declared is built are listed';

class C { has $!x is built; has $!y; has $.z }
is C.new(x => 1, z => 3).raku, 'C.new(x => 1, z => 3)',
    'a user class lists its `is built` private attribute';
is X::AdHoc.new(payload => "p").raku, 'X::AdHoc.new(payload => "p")', 'X::AdHoc';
