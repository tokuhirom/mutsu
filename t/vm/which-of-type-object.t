use Test;

# A type object's `.WHICH` text is rendered once per name (#17331); each
# call still answers a fresh ObjAt with the same identity.

plan 7;

class A { }
role R[::T = Any] { }

for ^2 {
    is A.WHICH, A.WHICH, 'a class answers one identity';
    isnt A.WHICH, Int.WHICH, 'two types answer two';
}
ok A.WHICH.Str.starts-with('A|U'), 'and renders its name';
is R.WHICH, R.WHICH, 'a role answers one identity';
ok (A.WHICH ~ '|' ~ Int.WHICH) eq (A.WHICH ~ '|' ~ Int.WHICH), 'stable as a cache key';
