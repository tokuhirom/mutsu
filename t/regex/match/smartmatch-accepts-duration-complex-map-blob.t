use Test;

# Smartmatch agrees with Rakudo's ACCEPTS (#13332): a Duration compares by
# value against a Numeric, a numeric Str matches a Complex, a Hash is not a
# Map, and a Blob topic against a Str dies X::Buf::AsStr.

plan 11;

ok Duration.new(5) ~~ 5, 'Duration topic ~~ Int';
ok 5 ~~ Duration.new(5), 'Int topic ~~ Duration';
nok Duration.new(5) ~~ 6, 'Duration topic ~~ other Int';
ok "5" ~~ 5+0i, 'numeric Str ~~ Complex';
nok "5" ~~ 5+1i, 'numeric Str ~~ Complex with imaginary part';
nok {a => 1} ~~ Map.new((a => 1)), 'Hash ~~ Map';
nok Map.new((a => 1)) ~~ {a => 1}, 'Map ~~ Hash';
ok Map.new((a => 1)) ~~ Map.new((a => 1)), 'Map ~~ Map';
ok {a => 1} ~~ {a => 1}, 'Hash ~~ Hash';
throws-like { "abc".ACCEPTS(Blob.new(1)) }, X::Buf::AsStr, 'Str.ACCEPTS(Blob)';
throws-like { Blob.new(1) ~~ "abc" }, X::Buf::AsStr, 'Blob ~~ Str';
