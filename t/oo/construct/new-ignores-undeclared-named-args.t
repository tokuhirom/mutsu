use Test;

plan 8;

# Raku's default BUILDALL only initialises DECLARED attributes; a named
# argument that names no attribute is silently ignored, not stashed on the
# object. Two objects that differ only in such a stray argument are `eqv`.

class C {
    has Int $.x;
}
is C.new(x => 1, bogus => 2).x, 1, 'an undeclared named arg is accepted';
ok C.new(x => 1) eqv C.new(x => 1, bogus => 2),
    'an undeclared named arg does not make the object different';
is C.new(x => 1, bogus => 2).^attributes.elems, 1,
    'no attribute is added for it';

# Splatting a hash of headers over several classes is the real-world shape
# (Cro::HTTP2::FrameParser passes `conn => …` to every frame class).
role R {
    has Int $.flags;
}
class D does R {
    has Blob $.data;
}
my %header = flags => 0, conn => 'connection-object';
ok D.new(data => Buf.new, |%header) eqv D.new(data => Buf.new, flags => 0),
    'a splatted extra named arg is ignored through a composed role';

# A class with a BUILTIN base keeps the permissive attribute bag: `Exception`
# holds `message`/`payload` that are declared nowhere in the registry.
class E is Exception {
    has $.code;
    method message() { "code $!code" }
}
is E.new(code => 7).message, 'code 7', 'an Exception subclass still builds';
is X::AdHoc.new(payload => 'boom').payload, 'boom',
    'a built-in exception type still takes its undeclared payload';

# Declared attributes are unaffected.
class F {
    has $.a;
    has @.b;
    has %.c;
}
my $f = F.new(a => 1, b => [2, 3], c => {d => 4}, nope => 5);
is "$f.a() {$f.b.join(',')} {$f.c<d>}", '1 2,3 4',
    'declared attributes of every sigil still build';
ok F.new(a => 1, nope => 5) eqv F.new(a => 1),
    'the stray arg does not survive alongside empty containers';
