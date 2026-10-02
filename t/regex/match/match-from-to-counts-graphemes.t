use v6.d;
use Test;

# Match.from/.to/.pos index the subject in graphemes (so "\r\n" is one
# character), the same unit Str.substr and Str.index use.
# Found via MVC::Keayl t/controller/params.rakutest (multipart bodies).

plan 8;

my $part = "Content-Disposition: x\r\n\r\nHello\r\n";
my $split = $part ~~ / \r?\n \r?\n /;
is $split.from, 22, '.from before a CRLF pair';
is $split.to, 24, '.to after two CRLF graphemes';
is $split.pos, 24, '.pos matches .to';
is $part.substr($split.to), "Hello\r\n", 'substr at .to lands on the body';

my $s = "ab\r\ncd";
my $m = $s ~~ /cd/;
is $m.from, 3, '.from counts CRLF as one character';
is $m.to, 5, '.to counts CRLF as one character';
is $s.index('cd'), $m.from, 'agrees with Str.index';

"a\nb" ~~ /b/;
is $/.from, 2, 'plain ASCII is unchanged';
