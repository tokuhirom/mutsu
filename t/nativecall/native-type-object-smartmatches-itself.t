use Test;

# LLM::RetrievalAugmentedGeneration::VectorDatabase: `when $_ ~~ num32 || $_ ~~ num64`
# on a `num32`/`num64` type object fell through to `default` and died, because a
# native type object smartmatched against its own native type was False (only a
# variable declared with that native type matched).

plan 13;

for (int, int8, int64, uint, uint8, num, num32, num64, str) -> $t {
    ok $t ~~ $t, "{$t.^name} type object matches itself";
}

my $t = num64;
ok $t ~~ num64, 'a variable holding the type object matches it';
nok $t ~~ num32, 'but not another native type';
ok num32 !~~ num64, 'two different native type objects do not match';

my $r = do given num32 {
    when 'num32' { 'string' }
    when $_ ~~ num32 || $_ ~~ num64 { 'native type' }
    default { 'other' }
}
is $r, 'native type', 'given on a native type object reaches the type branch';
