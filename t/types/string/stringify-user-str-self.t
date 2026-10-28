use v6;
use Test;

# Red (#15117): a user `method Str { self }` answers an object that cannot be
# unboxed to a native string. Rakudo dies in every native-string context;
# a lone interpolation hands the object back untouched.

plan 18;

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

# IO::Path segments stringify the same way (#15185).
for ('IO.add' => { "x".IO.add($f) }, 'IO.child' => { "x".IO.child($f) }) -> $case {
    try $case.value.();
    is ($!.defined ?? $!.message !! 'no exception'), $msg, $case.key;
}
is "x".IO.add(Bar.new).Str, 'x/bar', 'IO.add uses the user Str that answers a Str';
is "x".IO.child(Bar.new).Str, 'x/bar', 'IO.child uses the user Str that answers a Str';
dies-ok { "x".IO.sibling($f) }, 'IO.sibling dies for a Str that answers the object (rakudo words it as a coercion error)';
is "x".IO.sibling(Bar.new).Str, 'bar', 'IO.sibling uses the user Str that answers a Str';
