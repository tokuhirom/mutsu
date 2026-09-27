use Test;

# `Cool`'s string methods on an undefined `Cool` receiver (#9772). Every
# candidate raku declares for them takes a defined invocant, so a type object
# is `X::Multi::NoMatch` -- except the `Str:U` candidates of the case-mapping
# and counting methods, which stringify `Str` to "" with a warning. `Any` is
# not a `Cool` at all, so the sub forms on an undefined `Any` are
# `X::Method::NotFound`. Before, mutsu answered out of the type object's gist
# (`Str.comb` was `("(", "S", "t", "r", ")")`, `Str.chars` was 5).

plan 21;

for <lines words comb trim ords chomp contains index substr> -> $m {
    throws-like { Str."$m"() }, X::Multi::NoMatch, "Str.$m refuses the type object";
}
throws-like { Int.uc }, X::Multi::NoMatch, 'Int.uc refuses the type object';
throws-like { Int.chars }, X::Multi::NoMatch, 'Int.chars refuses the type object';
throws-like { Rat.comb }, X::Multi::NoMatch, 'Rat.comb refuses the type object';

quietly {
    is Str.chars, 0, 'Str.chars is 0';
    is Str.uc, '', 'Str.uc is the empty string';
    is Str.flip, '', 'Str.flip is the empty string';
    is uc(Str), '', 'uc(Str) is the empty string';
}

my $s;
throws-like { substr($s, 0, 2) }, X::Method::NotFound, 'substr on an undefined Any';
throws-like { substr(Str, 0, 1) }, X::Multi::NoMatch, 'substr on the Str type object';
throws-like { substr(Int, 0, 1) }, X::Multi::NoMatch, 'substr on the Int type object';
throws-like { comb(/./, Str) }, X::Multi::NoMatch, 'comb takes its subject second';
is comb(/./, "ab").join('|'), 'a|b', 'comb on a defined subject still works';
