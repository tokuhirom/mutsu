use Test;

# PDF::COS::TextString (`class ... { also is Str }`) passed to
# `sub utf16-encode(Str $str --> Str)`: the light call path refused any object
# for a builtin nominal type, so an instance of a class inheriting that
# builtin failed to bind.

plan 8;

class A is Str { }
class B { also is Str; has $.v = 1; method COERCE(Str $s) { self.new } }

sub plain(Str $s) { 'ok' }
sub returns-str(Str $s --> Str) { 'ok' }
sub with-named(Str $s, :$n) { 'ok' }
sub as-int(Str $s --> Int) { 1 }

is plain(A.new), 'ok', 'a class that is Str binds a Str parameter';
is returns-str(B.new), 'ok', 'a class with also is Str, with a return type';
is with-named(A.new), 'ok', 'a signature with a named parameter';
is as-int(B.new), 1, 'another return type';
is B.new.&returns-str(), 'ok', 'as a method-call-form argument';

sub only-int(Int $i) { 'ok' }
throws-like { only-int(A.new) }, X::TypeCheck::Binding::Parameter, 'an unrelated builtin is still refused';
throws-like { returns-str(Any.new) }, X::TypeCheck::Binding::Parameter, 'so is an unrelated object';

class Plain { }
throws-like { plain(Plain.new) }, X::TypeCheck::Binding::Parameter, 'and a plain user class';
