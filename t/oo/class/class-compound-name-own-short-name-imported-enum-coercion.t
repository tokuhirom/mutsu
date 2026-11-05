use v6;
use lib 't/lib-compound-own-short-enum';
use Test;
use CompoundOwnShortEnumCodes;

# Universal::Errno: `class X::Errno` calls `Errno($code)` for the enum it
# `use`d. The class's own short name `Errno` (registered in its compound
# name's prefix package `X`) is no binding inside it, so the call reaches the
# imported enum instead of recursing into `X::Errno.new`.

plan 5;

class X::Errno is Exception {
    has Errno $.errno;
    multi method new(Int() $code) { my $errno = Errno($code); self.bless(:$errno) }
    multi method new(Errno $errno) { self.bless(:$errno) }
    method coerce($c) { Errno($c) }
}

is X::Errno.new(2).errno, ENOENT, 'Int() candidate coerces through the imported enum';
is X::Errno.new(ENOENT).errno, ENOENT, 'enum candidate';
is X::Errno.coerce(1), EPERM, 'a plain method call';
is Errno(2), ENOENT, 'from the unit';
is X::Errno.new(1).errno.^name, 'Errno', 'the attribute type is the enum';

done-testing;
