# From CSS::Writer: prefix `~` calls `.Str`; a class whose only catch-all is
# `FALLBACK($name, $val, |c)` must not see a `Stringy` call reach it.
use Test;

plan 4;

class W {
    has $.v = 'zzz';
    method Str { $!v }
    method FALLBACK($name, $val, |c) { "fallback:$name" }
}

my W $w .= new;
is ~$w, 'zzz', 'prefix ~ uses .Str, not FALLBACK';
is "$w", 'zzz', 'interpolation uses .Str';
is $w.write-foo(1), 'fallback:write-foo', 'FALLBACK still answers unknown methods';

class S {
    method Stringy { 'stringy' }
    method FALLBACK($name, $val, |c) { 'fb' }
}
is ~S.new, 'stringy', 'a user Stringy still wins';
