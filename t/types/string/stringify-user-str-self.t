use v6;
use Test;

# Red (#15117): a user `method Str { self }` answers an object that cannot be
# unboxed to a native string. Rakudo dies in every native-string context;
# a lone interpolation hands the object back untouched.

plan 12;

class Foo { has $.n; method Str { self } }
my $f = Foo.new(n => 1);
my $msg = 'This type cannot unbox to a native string: P6opaque, Foo';

for ('"x" ~ $f' => { "x" ~ $f }, '$f ~ "x"' => { $f ~ "x" },
     '"a{$f}b"' => { "a{$f}b" }, '"a$f"' => { "a$f" },
     '[$f].join(",")' => { [$f].join(",") }, 'join(",", $f)' => { join(",", $f) },
     'print $f' => { print $f }, 'put $f' => { put $f }) -> $case {
    try $case.value.();
    is ($!.defined ?? $!.message !! 'no exception'), $msg, $case.key;
}

isa-ok "$f", Foo, 'a lone "$f" is the object itself';
isa-ok "{$f}", Foo, 'a lone "{$f}" is the object itself';
isa-ok ~$f, Foo, 'prefix ~ does not unbox';
class Bar { method Str { "bar" } }
is "<{Bar.new}>", '<bar>', 'a user Str answering a Str still stringifies';
