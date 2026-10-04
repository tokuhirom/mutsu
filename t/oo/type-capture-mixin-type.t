use Test;

# A `::T` type capture binds the argument's `.WHAT`. For a role-mixed value
# that is the composed `C+{R}` type, not the base value's type name.

plan 7;

role R { method tag { 'r' } }
class C { }

my sub cap(Mu ::T) { T }

my \M = C.^mixin(R);
is cap(M).^name, 'C+{R}', 'a mixin type object captures as itself';
is cap(M.new).^name, 'C+{R}', 'an instance of a mixin type captures its type';
is cap(C.new but R).^name, 'C+{R}', '`but` on an instance';
is cap(1 but R).^name, 'Int+{R}', '`but` on a native value';
ok cap(M) === M.WHAT, 'the capture is the same type object .WHAT answers';

# A class whose `^parameterize` returns a mixin (upstream NativeCall's
# `CArray[T]` works this way).
role Typed[::E] { method of { E } }
class Box {
    method ^parameterize(Mu:U \b, Mu:U \t) {
        my $w := b.^mixin(Typed[t]);
        $w.^set_name("Box[{t.^name}]");
        $w
    }
}
is cap(Box[Int]).^name, 'Box[Int]', 'a ^parameterize mixin captures under its set name';
is cap(Box[Int]).of.^name, 'Int', 'the captured type keeps the role methods';
