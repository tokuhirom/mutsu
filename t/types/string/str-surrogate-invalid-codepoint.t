use v6.d;
use Test;

plan 20;

# A Str never holds a surrogate codepoint (U+D800..U+DFFF): every
# construction path dies with X::InvalidCodepoint (#14416).
my @paths =
    'chr'     => { chr(0xD800) },
    'Int.chr' => { 0xDFFF.chr },
    'chrs'    => { chrs(65, 0xD800) },
    'Uni.new' => { Uni.new(0xD800).Str },
    'esc-x'   => { EVAL q["\x[D800]"] },
    'esc-o'   => { EVAL q["\o[154000]"] },
    'esc-c'   => { EVAL q["\c[55296]"] },
    'esc-cd'  => { EVAL q["\c55296"] };

for @paths -> $p {
    my $r = try { $p.value.() };
    isa-ok $!, X::InvalidCodepoint, "{$p.key} dies with X::InvalidCodepoint";
}

try { chr(0xD800) };
is $!.code, 55296, 'the exception carries the code';
is $!.message, 'Invalid surrogate codepoint 55296', "... and Rakudo's message";

try { Uni.new(0x110000) };
isa-ok $!, X::InvalidCodepoint, 'Uni.new past Unicode dies with X::InvalidCodepoint';
is $!.message, 'Invalid codepoint 1114112; must not exceed 0x10ffff (1114111)',
    '... with the out-of-range message';

try { chrs(65, 0x110000) };
is $!.message, 'chr codepoint 1114112 (0x110000) is out of bounds', 'chrs past Unicode is out of bounds';

try { chrs(-1) };
is $!.message, 'chr codepoint -1 cannot be negative', 'chrs of a negative codepoint';

# The neighbours of the surrogate block are still fine.
is chr(0xD7FF).ord, 0xD7FF, 'U+D7FF is a character';
is chr(0xE000).ord, 0xE000, 'U+E000 is a character';
is chrs(65, 66).Str, 'AB', 'chrs still builds a string';
is Uni.new(0x61, 0x62).Str, 'ab', 'Uni.new still builds a Uni';
is "\x[41]\c[66]\o[103]", 'ABC', 'numeric escapes still decode';
is-deeply (try chr(0xD800)), Nil, 'the failed chr yields Nil under try';
