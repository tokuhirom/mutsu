use v6;
use Test;

# A bareword listop call may take a bareword fat-arrow pair (`key => value`) as
# a no-paren argument: `samewith key => $v` parses as `samewith(key => $v)`.
# Previously only a colonpair (`:$v`) or a term starting with a sigil/quote/digit
# was recognized as a no-paren argument, so a bareword pair key desynced the
# parser ("Two terms in a row" / "expected expression"). Regression surfaced by
# Digest's HMAC.rakumod (`samewith key => $key.encode, :$msg, :&hash, :$block-size`).

plan 6;

# samewith redispatch with a fat-arrow named argument (terminates by narrowing)
proto f(|) { * }
multi f(Int :$n)    { samewith name => "num-$n" }
multi f(Str :$name) { $name }
is f(n => 5), "num-5", "samewith key => value redispatches with a Pair arg";

# multiple bareword pairs are all collected as arguments
sub collect(*%h) { %h.sort.map({ "{.key}={.value}" }).join(",") }
is collect(a => 1, b => 2, c => 3), "a=1,b=2,c=3",
    "no-paren call collects multiple bareword fat-arrow pairs";

# a bareword pair mixed with colonpairs and other args
sub mix(*@pos, *%named) {
    @pos.join(",") ~ " | " ~ %named.sort.map({ .key ~ "=" ~ .value }).join(",")
}
my $msg = "M";
is mix(1, key => 2, :$msg), "1 | key=2,msg=M",
    "bareword pair mixes with positional and colonpair args";

# hyphenated pair key works too
sub h(*%x) { %x<block-size> }
is h(block-size => 64), 64, "hyphenated bareword pair key";

# a lone bareword pair (single arg, no trailing comma)
sub one(*%x) { %x<only> }
is one(only => 'yes'), 'yes', "single bareword pair as the only argument";

# `key => val` immediately after the name (no space) still a Pair, not a call arg
my $p = (thing => 9);
is $p.key, 'thing', "tight fat-arrow still forms a Pair";
