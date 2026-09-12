use Test;

# `×` (U+00D7) and `÷` (U+00F7) are spellings of `*` and `/`: rakudo makes
# `&infix:<×> === &infix:<*>`. A user `multi infix:<×>` therefore sits in FRONT
# of that shared core implementation rather than replacing it, and the
# assignment metaop `×=` is built over the name as spelled.
#
# mutsu diverts `×` off `OpCode::Mul` as soon as any user `infix:<×>` exists, so
# both halves were wrong: a declined user candidate fell through to the lenient
# reduction fallback (reading an object as 0 instead of throwing), and `×=`
# desugared to `*=`, which looks up a different name. From Math::Vector.

plan 10;

class Vec3 {
    has @.components;
    multi method new(*@x) { self.bless(components => @x) }
    method dim() { @.components.elems }
    method scale($by) { Vec3.new(@.components >>*>> $by) }
}

multi infix:<×>(Vec3 $a where { $a.dim == 3 }, Vec3 $b where { $b.dim == 3 }) is export {
    'crossed'
}
multi infix:<÷>(Vec3 $a where { $a.dim == 3 }, $b) is export { $a.scale(1 / $b) }

my $three = Vec3.new(1, 2, 3);
my $five  = Vec3.new(1, 2, 3, 4, 5);

is $three × $three, 'crossed', 'the user × candidate takes the call';
is ($three ÷ 2).components.join(','), '0.5,1,1.5', 'the user ÷ candidate takes the call';

# No user candidate matches: the CORE candidate of `×` is `*`'s, so an operand
# with no numeric coercion throws exactly as `*` does.
dies-ok { $three × $five }, 'a declined × candidate falls back to core * (throws)';
dies-ok { $five ÷ 2 }, 'a declined ÷ candidate falls back to core / (throws)';

# The core spellings keep working on numbers even with the user candidates around.
is 6 × 7, 42, 'numeric × still multiplies';
is 12 ÷ 4, 3, 'numeric ÷ still divides';

# `×=` / `÷=` are the assignment metaop over the name AS SPELLED, so they reach
# the user candidate.
{
    my $v = Vec3.new(1, 2, 3);
    $v ÷= 2;
    is $v.components.join(','), '0.5,1,1.5', '÷= reaches the user infix:<÷>';
}
{
    my $x = Vec3.new(1, 2, 3);
    my $r = $x × $x;
    is $r, 'crossed', 'and the bare operator is unchanged by that';
}

# With no user candidate in sight, `×=` / `÷=` are still plain `*=` / `/=`.
{
    my $n = 3;
    $n ×= 4;
    is $n, 12, '×= on a number is *=';
    my $m = 12;
    $m ÷= 4;
    is $m, 3, '÷= on a number is /=';
}
