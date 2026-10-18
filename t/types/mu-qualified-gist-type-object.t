use Test;

plan 4;

role R {
    multi method gist(::?ROLE:U:) { self.Mu::gist }
}
class A::Foo does R { }
class B { method g { self.Mu::gist } }

is A::Foo.gist, '(Foo)', 'a role gist delegating to Mu::gist shows the short name';
is A::Foo.Mu::gist, '(Foo)', 'Mu::gist on a namespaced type object is (ShortName)';
is B.g, '(B)', 'Mu::gist on a plain type object';
is A::Foo.raku, 'A::Foo', '.raku keeps the full name';
