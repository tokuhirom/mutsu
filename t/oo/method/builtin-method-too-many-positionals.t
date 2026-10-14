use Test;

plan 8;

# A built-in method called with more positionals than any candidate takes
# fails to bind (X::AdHoc), it is not "No such method".
my $msg = 'Too many positionals passed; expected 1 argument but got 2';

throws-like { "x".uc(1) }, X::AdHoc, :message($msg), 'Str.uc(1)';
throws-like { 5.sqrt(2) }, X::AdHoc, :message($msg), 'Int.sqrt(2)';
throws-like { "3".Int(1) }, X::AdHoc, :message($msg), 'Str.Int(1)';
throws-like { "1".Rat(0.1) }, X::AdHoc, :message($msg), 'Str.Rat(0.1)';

# A method the receiver does not have is still X::Method::NotFound.
throws-like { "x".no-such-method(1) }, X::Method::NotFound, 'unknown method';
throws-like { 5.uc-nope }, X::Method::NotFound, 'unknown method, no args';

# Valid argument counts are untouched.
is "abc".uc, "ABC", 'uc()';
is 5.sqrt.round(0.01), 2.24, 'round($scale)';
