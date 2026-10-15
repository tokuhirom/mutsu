use v6.d;
use Test;

# Rakudo checks a private method's trust at the `!` call site only. A private
# method OBJECT (`^find_private_method`) called as a routine
# (`$type.&handle()`) from code in a package the owner does not trust is legal;
# AttrX::Mooish's `invoke-composer` does exactly that.
plan 4;

my class Owner {
    method !secret { 'secret' }
    submethod !hidden (*%named) { 'hidden' }
}

role Caller {
    method via-method-object(Mu \type) {
        my &handle = type.^find_private_method('secret');
        type.&handle()
    }
    method via-submethod-object(Mu \type) {
        my &handle = type.^find_private_method('hidden');
        type.&handle()
    }
}
class Other does Caller { }

is Other.via-method-object(Owner), 'secret', 'a private method object is callable from an untrusted role';
is Other.via-submethod-object(Owner), 'hidden', 'so is a private submethod object';
is Owner.new.&{ my &h = Owner.^find_private_method('secret'); $^o.&h() }, 'secret',
    'and from plain code';

throws-like { EVAL 'class A { method !s { 1 } }; class B { method m(A $a) { $a!A::s } }; B.m(A.new)' },
    X::Method::Private::Permission,
    'the call-site check of a fully qualified private call is unchanged';
