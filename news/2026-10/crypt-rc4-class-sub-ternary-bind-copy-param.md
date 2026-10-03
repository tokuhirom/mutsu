# `$?CLASS` in class-body subs, binds through a conditional, and untyped `is copy` arrays

Crypt::RC4 fell over on three unrelated gaps. Its suite now passes 12/12; it
used to die before its first test.

- **`$?CLASS` in a class-body `sub`** was read from the environment when the
  sub was called, so it named whichever class body had run last. In
  Crypt::RC4's case that was a class from `Test`, which gave
  "No such method 'RC4' for invocant of type 'Test::X::SubtestsSkipped'". The
  compiler now knows the enclosing class while it compiles a class body's
  statements, and turns `$?CLASS` in a sub or closure there into that class as
  a constant. Methods keep their own `$?CLASS`, and a role method still sees
  the class that consumes the role.
- **Binding to a conditional** (`my $x := $c ?? @a[$i] !! @a[$j]`) kept only
  the value, so a later assignment died with "Cannot assign to an immutable
  value". A conditional whose branches are all containers now binds the
  branch it selects, the same way `my $x := @a[$i]` does.
- **An `@b is copy` parameter** kept a native or typed argument's element
  type. `my uint8 @buf` therefore failed `--> Array` with "expected Array but
  got Array". The copy is now always a fresh, untyped `Array`, as rakudo makes
  it. One helper, `Value::into_param_copy`, now builds that copy for positional,
  named and default arguments, and for the method fast path, which had been
  doing only a shallow detach.
