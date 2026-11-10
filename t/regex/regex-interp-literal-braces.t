use Test;

plan 12;

# `<$re>` interpolates a string as regex source. A `{` that only stands for the
# character (single-quoted, backslash-escaped, in a `<[...]>` class) is not a
# code block and is accepted. (A real code block is still refused; that is
# covered by roast/S05-interpolation/regex-in-variable.t, including an unbalanced
# `}` that ends an injected block.)

sub interp(Str $src, Str $target) {
    my $re = $src;
    my $rx = rx/<$re>/;
    so $target ~~ $rx
}

ok interp(Q/'{'/, 'a{b}'), 'quoted open brace';
ok interp(Q/'}'/, 'a{b}'), 'quoted close brace';
ok interp(Q/'a{b}'/, 'a{b}'), 'braces inside one quoted literal';
ok interp(Q/'a' '{' ',' '3' '}'/, 'a{,3}'), 'LIVR style translation of a{,3}';
ok interp(Q/\{/, 'a{b}'), 'escaped open brace';
ok interp(Q/a \{ b/, 'a{b'), 'escaped brace between atoms';
ok interp(Q/<[{]>/, 'a{b}'), 'brace in a character class';
ok interp(Q/<[{}]>/, 'a{b}'), 'both braces in a character class';
ok interp(Q/<-[{]>/, 'a{b}'), 'brace in a negated class';
ok interp(Q/'\{'/, 'a\{b'), 'escaped brace inside a quoted literal';
ok interp(Q/ <[a..z]> '{'/, 'a{b}'), 'class followed by a quoted brace';
ok interp(Q/ "x" '{'/, 'x{'), 'double-quoted literal followed by a quoted brace';

done-testing;
