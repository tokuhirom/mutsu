use Test;

# `X but R<words>` / `X does R<words>` is rakudo's spelling of
# `X but R(<words>)`: the role plus its attribute's initial value, although
# `R<words>` on its own is just `Any`. Reduced from Needle::Compile.

plan 6;

my role Type { has $.type }

is ("foo" but Type<words>).type, "words", 'but R<word> initializes the attribute';
ok ("foo" but Type<words>) ~~ Type, '...and mixes the role in';
is-deeply ("x" but Type<a b>).type, ("a", "b"), 'but R<a b> passes the word list';
is (42 but Type<n>).type, "n", 'works on a non-Str invocant';
is ("x" does Type<z>).type, "z", 'does R<word> on a value';
my $v = "q";
$v does Type<z>;
is $v.type, "z", 'does R<word> on a variable';
