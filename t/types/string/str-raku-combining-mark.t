use Test;

plan 5;

is "\x[301]x".raku, '"\x[301]x"', 'combining mark at string start is escaped';
is "a\n\x[301]x".raku, '"a\n\x[301]x"', 'combining mark after \n is escaped';
is "a\x[301]x".raku, "\"a\x[301]x\"", 'combining mark after a base is kept';
is "\x[200D]a".raku, '"\x[200D]a"', 'ZWJ at string start is escaped';
is "a\r\n\x[301]".raku, '"a\r\n\x[301]"', 'combining mark after \r\n is escaped';
