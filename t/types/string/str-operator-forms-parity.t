use v6;
use Test;

# Every form of a string operator is one routine (ADR-0118 §2.6): the
# opcode, the `&infix:<op>` routine form (what `.sort(&infix:<leg>)` and
# `cmp-ok` call), and the metaop forms all coerce their operands the same
# way, and every place a Blob is asked for a string throws the one
# X::Buf::AsStr. Expected values were measured with rakudo.

plan 29;

class S { method Str { "b" } }
my $s = S.new;

# leg honours a user Str, like eq / lt always did.
is ($s leg "a"), More, 'leg calls a user Str (it compared the .gist)';
is infix:<leg>($s, "a"), More, '... and so does &infix:<leg>';
is ($s, "a").sort(&infix:<leg>).map(~*).join(','), 'a,b', '... and a sort by it';
is (($s,) »leg« ("a",)).head, More, '... as the hyper form already did';
is (any(1, 3) leg 2).raku, any(Less, More).raku, 'leg autothreads a junction (it answered More)';

# The routine forms of the string comparators and ~.
ok infix:<eq>($s, "b"), '&infix:<eq> calls a user Str';
ok infix:<lt>($s, "c"), '&infix:<lt>';
ok infix:<gt>($s, "a"), '&infix:<gt>';
nok infix:<ne>($s, "b"), '&infix:<ne>';
is infix:<~>($s, "x"), 'bx', '&infix:<~> calls a user Str';
ok infix:<eq>("hi".encode, Buf.new(104, 105)), '&infix:<eq> keeps the Blob-pair byte compare';

# Blob stringification: only utf8 decodes; any other Blob dies with
# X::Buf::AsStr, named after the method that wanted the Str.
sub dies-as(&code, $method, $desc) {
    try { sink code() }
    ok ($! ~~ X::Buf::AsStr && $!.method eq $method), $desc;
}
dies-as { Buf.new(1) ~ "a" }, 'Stringy', 'Buf ~ Str dies (it concatenated the bytes)';
dies-as { "a" ~ Buf.new(1) }, 'Stringy', 'Str ~ Buf dies';
dies-as { ~Buf.new(1) }, 'Stringy', 'prefix ~ of a Buf dies (it answered its gist)';
dies-as { "{Buf.new(1)}" }, 'Stringy', 'interpolating a Buf dies naming Stringy';
dies-as { infix:<~>("a", Buf.new(1)) }, 'Stringy', '&infix:<~> with a Buf dies';
dies-as { Buf.new(1) leg "a" }, 'Stringy', 'Buf leg Str dies';
dies-as { Buf.new(1) leg Buf.new(2) }, 'Stringy', 'Buf leg Buf dies (leg has no Blob candidate)';
dies-as { Buf.new(1) eq "a" }, 'Stringy', 'Buf eq Str dies naming Stringy';
dies-as { Buf.new(1).Str }, 'Str', '.Str names Str';
dies-as { Buf.new(1).chars }, 'chars', '.chars names chars';
is "a" ~ "b".encode, 'ab', 'Str ~ utf8 decodes';
is ~"b".encode, 'b', 'prefix ~ of a utf8 decodes';
is "{"b".encode}", 'b', 'interpolating a utf8 decodes';
is ("hi".encode leg "hi"), Same, 'utf8 leg Str decodes';

# The message is rakudo's, word-wrapped at 72 columns.
try { sink Buf.new(1) ~ "a" }
is $!.message, "Stringification of a Buf is not done with 'Stringy', which the '~'\n"
    ~ "operator uses. The 'decode' method should be used to convert a Buf to a\nStr.",
    'the Stringy message';
try { sink Buf.new(1).Str }
is $!.message, "Stringification of a Buf is not done with 'Str'. The 'decode' method\n"
    ~ "should be used to convert a Buf to a Str.", 'the Str message';
try { sink Buf.new(1).chars }
is $!.message, "A Buf is not a Str, so using 'chars' will not work. The 'decode' method\n"
    ~ "should be used to convert a Buf to a Str.", 'the message for another method';
is X::Buf::AsStr.new(:object(Blob.new), :method<Str>).message.lines[0],
    "Stringification of a Blob is not done with 'Str'. The 'decode' method",
    'a hand-built X::Buf::AsStr renders the same text';
