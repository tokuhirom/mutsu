use Test;

plan 3;

# An exception raised by a user `trait_mod:<is>` candidate fails the method's
# declaration.

my multi sub trait_mod:<is>(Method:D $method, :$refuse!) {
    die "Cannot apply 'is refuse' to method '{$method.name}'"
        unless $method.package.HOW.WHAT =:= Metamodel::ClassHOW;
}

throws-like { 'role A { method b is refuse { } }'.EVAL },
  X::AdHoc,
  message => "Cannot apply 'is refuse' to method 'b'";

lives-ok { 'class B { method b is refuse { } }'.EVAL },
  "the handler accepts a class method";

throws-like { 'role C { method c() is refuse { 1 } }'.EVAL },
  X::AdHoc,
  message => "Cannot apply 'is refuse' to method 'c'";

done-testing;
