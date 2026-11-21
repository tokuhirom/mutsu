use Test;

# An anonymous regex declarator may carry a `my` scope and a signature:
# `my regex ($x) { $x }` is the same term as `regex ($x) { $x }`. mutsu
# refused it with "Two terms in a row".

plan 4;

my $p = my regex ($x) { $x };
is ("b" ~~ / <&$p("b")> /).Str, 'b', 'a my regex with a signature';

my $q = my regex { b };
is ("b" ~~ $q).Str, 'b', 'a my regex with no signature';

my $t = my token ($x) { $x };
is ("ab" ~~ / a <&$t("b")> /).Str, 'ab', 'a my token with a signature';

my regex named { c };
is ("c" ~~ / <named> /).Str, 'c', 'a named my regex still declares its name';
