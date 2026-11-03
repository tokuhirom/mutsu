use Test;

# `GLOBAL::<$x>` is answered for that one key (#9171). The answer must be what
# the whole `GLOBAL::` stash holds under the key, for every kind of root
# variable: the two reads are compared on each.

plan 13;

our $os = 1;
our @oa = 1, 2, 3;
our %oh = a => 1;
my $lex = 'lexical';

is GLOBAL::<$os>, 1, 'an `our` scalar';
is GLOBAL::<@oa>, (1, 2, 3), 'an `our` array';
is GLOBAL::<%oh><a>, 1, 'an `our` hash';
ok GLOBAL::<$os>:exists, ':exists on a present key';
nok GLOBAL::<$no-such-var>:exists, ':exists on a missing key';
nok GLOBAL::<@no-such-array>:exists, ':exists on a missing array key';
nok GLOBAL::<$lex>:exists, 'a `my` lexical is not a GLOBAL member';

for <$os @oa %oh $lex $no-such-var> -> $key {
    is-deeply GLOBAL::{$key}:exists, GLOBAL::.keys.grep($key).so,
        "keyed {$key.raku} agrees with the whole stash";
}

GLOBAL::<$os> = 5;
is $os, 5, 'a write through the keyed entry lands in the variable';
