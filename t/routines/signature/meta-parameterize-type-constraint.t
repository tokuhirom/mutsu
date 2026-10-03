use Test;

# A type constraint `C[T]` where `C` declares `method ^parameterize` (as
# upstream NativeCall's `CArray` does) stands for the type that meta-method
# builds, so a value of `C[T]` binds to it and a value of `C[U]` does not.

plan 8;

role Elems[::T] { method of { T } }
class C {
    method ^parameterize(Mu:U \c, Mu:U \t) {
        my $what := c.^mixin(Elems[t]);
        $what.^set_name(c.^name ~ '[' ~ t.^name ~ ']');
        $what
    }
}

my $s = C[Str].new;
ok $s ~~ C[Str], 'smartmatch against the same parameterization';
nok $s ~~ C[Int], 'but not against another one';
nok C.new ~~ C[Str], 'nor is the bare class a parameterization';

sub takes-str(C[Str] $x) { 'str' }
is takes-str($s), 'str', 'a parameter typed C[Str] accepts a C[Str]';
dies-ok { takes-str(C[Int].new) }, 'and rejects a C[Int]';

my C[Str] $v = $s;
ok $v === $s, 'a variable typed C[Str] accepts it';
dies-ok { my C[Int] $w = $s }, 'a variable typed C[Int] does not';

# The same holds when the class comes from a constant alias.
my constant Alias = C;
sub takes-alias(Alias[Str] $x) { 'alias' }
is takes-alias($s), 'alias', 'a constant alias of the class parameterizes too';
