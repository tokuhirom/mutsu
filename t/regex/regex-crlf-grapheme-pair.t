use Test;

plan 8;

# CR LF is one grapheme. `\r` and `\n` written as adjacent escapes denote the
# whole pair (as rakudo does), but whitespace between them, or right after the
# `\n`, splits them into two atoms that each fail to match half a grapheme.
my $s = "ab\r\ncd";

nok so($s ~~ /b \r\n c/), 'whitespace after the pair splits it (rakudo)';
nok so($s ~~ /b\r\n c/), 'whitespace after \n splits the pair';
nok so($s ~~ /\r \n/), 'whitespace between \r and \n splits the pair';
ok so($s ~~ /b\r\nc/), 'adjacent \r\n matches CRLF';
ok so($s ~~ /b \r\nc/), 'whitespace before the pair keeps it intact';
ok so($s ~~ /b \n c/), 'a lone \n still matches CRLF';
nok so($s ~~ /b \r c/), 'a lone \r cannot match half of CRLF';
ok so($s ~~ /\r\n/), 'bare \r\n matches';

done-testing;
