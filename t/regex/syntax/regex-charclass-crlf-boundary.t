use v6;
use Test;

my $crlf = "\r\n";

ok $crlf ~~ /<[\n]>/, 'logical newline class matches a CRLF grapheme';
nok $crlf ~~ /<[\x[0A]]>/, 'raw LF does not match inside a CRLF grapheme';
nok $crlf ~~ /<[\x0A..\x1F]>/, 'a raw codepoint range does not match CRLF';
nok $crlf ~~ /<- [\n]>/, 'negated logical newline class excludes CRLF';
ok $crlf ~~ /<- [\x[0A]]>/, 'negated raw LF class accepts CRLF';
ok $crlf ~~ /<- [\x0D..\x1F]>/, 'negated raw range accepts CRLF';
ok $crlf ~~ m:m{^ <[\x00..\x7F]>+ $}, ':ignoremark classes test the CRLF base codepoint';
ok $crlf ~~ m:m{^ <[\x0D]> $}, ':ignoremark raw CR class matches the CRLF base';
nok $crlf ~~ m:m{^ <[\x0A]> $}, ':ignoremark raw LF class does not match the CRLF base';
ok $crlf ~~ m:m{^ <[\n]> $}, ':ignoremark logical newline class matches CRLF';

done-testing;
