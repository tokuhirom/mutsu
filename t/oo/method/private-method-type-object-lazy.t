use Test;

# A private method call written inside a class is allowed when it runs later,
# outside the method, for a type-object invocant as well as for an instance:
# `T.all` returns `gather { self!all }`, whose body runs when the Seq is read
# (Trie's `get-all` on a missing key). It used to die with "Cannot call
# private method 'all' on package T because it does not trust GLOBAL".

plan 3;

class T {
    method all { gather { self!all } }
    method !all { take 1; take 2 }
}

is-deeply T.new.all.List, (1, 2), 'an instance invocant';
is-deeply T.all.List, (1, 2), 'a type-object invocant';

throws-like 'class Other { method poke { T!T::all } }; Other.poke',
    X::Method::Private::Permission,
    'a class that T does not trust is still refused';
