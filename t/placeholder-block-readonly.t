use Test;

# A scalar placeholder parameter is a readonly alias. This is distinct from
# the implicit topic of a bare block: the placeholder remains readonly even
# when the callback receives a real array element.
plan 5;

throws-like { my $b = { $^x = 9 }; $b(1) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'a scalar placeholder parameter rejects assignment';

throws-like { my $v = 1; my $b = { $^x = 9 }; $b($v) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'a scalar placeholder parameter rejects assignment from a variable';

throws-like { my @a = 1, 2; @a.map({ $^x = 9 }).eager }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'a map fast loop keeps scalar placeholder parameters readonly';

throws-like { my @a = 1, 2; @a.grep({ $^x = 9 }).eager }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'a grep fast loop keeps scalar placeholder parameters readonly';

throws-like { my @a = 1, 2; @a.first({ $^x = 9 }) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'a first fast loop keeps scalar placeholder parameters readonly';
