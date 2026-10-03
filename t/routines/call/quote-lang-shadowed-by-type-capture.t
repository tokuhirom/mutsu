use Test;

plan 4;

sub captured(::S $value) { S.^name }
is captured(42), 'Int', 'a signature type capture shadows the S quote language';

role Tree[::Elem] {
    has Tree[Elem] $.left;
    method e { Elem.^name }
}
class C {
    method w(::S:) { S.^name }
}

is Tree[Int].new.e, 'Int', 'a later chained call parses after a method type capture';
is C.w, 'C', 'an invocant type capture remains available in its method body';
is captured('x'), 'Str', 'the capture binds independently for each call';
