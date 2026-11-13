use Test;

plan 4;

# A wrap handle kept in a `:=`-bound attribute unwraps (Memoize's `Memoized`
# role binds `$!unwrap-handle := unwrap-handle`).

my role M { has $.h; method set($h) { $!h := $h; self } }
proto sub mem(|) {*}
multi sub mem(&code) { (&code does M).set(&code.wrap(-> |c { callsame() + 1 })) }
proto sub unmem(|) {*}
multi sub unmem(M:D $code) { $code.unwrap($code.h); 'ok' }

sub b($x) { $x * 2 }
mem(&b);
is b(3), 7, 'the wrapper runs';
is unmem(&b), 'ok', 'the bound handle unwraps';
is b(3), 6, 'the original runs again';

my $h := &b.wrap(-> |c { callsame() + 10 });
&b.unwrap($h);
is b(3), 6, 'a directly bound handle unwraps too';
