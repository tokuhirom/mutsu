use v6.c;
use Test;

plan 3;

throws-like { my Int:D $x .= new: 42 }, X::AdHoc,
    message => /'You cannot create an instance of this type (Int:D)'/,
    'v6.c declaration calls new on the constrained type';

throws-like { my Int:U $x .= new: 42 }, X::AdHoc,
    message => /'You cannot create an instance of this type (Int:U)'/,
    'v6.c preserves either definiteness smiley';

throws-like { Int:D.new }, X::AdHoc,
    message => /'You cannot create an instance of this type (Int:D)'/,
    'a constrained type object cannot be instantiated directly';
