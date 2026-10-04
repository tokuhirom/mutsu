use Test;

# A role type object cannot be instantiated until it is punned, so its
# `.REPR` is `Uninstantiable` (rakudo's ParametricRoleGroupHOW /
# CurriedRoleHOW). A class, and a punned role's instance, stay `P6opaque`.

plan 8;

role R { }

is R.REPR, 'Uninstantiable', 'a user role';
is Positional.REPR, 'Uninstantiable', 'a core role';
is Positional[Int].REPR, 'Uninstantiable', 'a curried core role';
is Blob.REPR, 'Uninstantiable', 'Blob';
is Buf[uint8].REPR, 'Uninstantiable', 'a curried Buf';
is Array[Int].REPR, 'P6opaque', 'a parameterized class is not a role';
is R.new.REPR, 'P6opaque', 'a punned role instance';
is Buf.new.REPR, 'VMArray', 'a Buf instance';
