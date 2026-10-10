use Test;

# `throw` of an Exception / X:: instance and of a Failure are rows of the method
# table (ADR-11276 §9.54).

plan 6;

my $f = Failure.new("oops");
try { $f.throw; CATCH { default { is .WHAT.raku, 'X::AdHoc', 'Failure.throw raises the wrapped exception'; is .message, 'oops', 'its message' } } }
try { X::AdHoc.new(:payload("boom")).throw; CATCH { default { is .message, 'boom', 'X::AdHoc.throw uses the payload' } } }
try { X::TypeCheck::Assignment.new(:got(1), :expected(Str)).throw; CATCH { default { is .WHAT.raku, 'X::TypeCheck::Assignment', 'typed X:: throw' } } }
class E2 is Exception { method message { "lazy" } }
try { E2.new.throw; CATCH { default { is .WHAT.raku, 'E2', 'user subclass throws itself'; is .message, 'lazy', 'computed message kept' } } }
