use Test;
plan 13;

is 0xFF, 255, '0xFF is 255';
is 0x10, 16, '0x10 is 16';
is 0o77, 63, '0o77 is 63';
is 0o10, 8, '0o10 is 8';
is 0b1010, 10, '0b1010 is 10';
is 0b11111111, 255, '0b11111111 is 255';
is 0b༡༠༡༠༡༠, 42, 'Unicode Nd digits work in binary literals';
is 0o᠗᠕᠕, 493, 'Unicode Nd digits work in octal literals';
is 0xＣＡＦＥ, 51966, 'fullwidth hexadecimal letters are accepted';
is :36<Ｕｎｉｃｏｄｅｚ>, 2402100600299, 'fullwidth letters work in generic radix literals';
throws-like "say 0o7₅₅", X::Syntax::Confused, "No numerals are rejected in octal literals";
throws-like "say :36<αω>", X::Syntax::Malformed, "non-Hex_Digit scripts are rejected in generic radix literals";
throws-like { "௰".Int }, X::Str::Numeric, 'No numerals are not accepted by Str.Int coercion';
