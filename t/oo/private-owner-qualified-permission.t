use Test;

plan 1;

class A {
    method !p() { 42 }
}

class B { }

my $_ = B.new;
throws-like '$_!A::p()', X::Method::Private::Permission,
    'owner-qualified private call rejects unrelated invocant';
