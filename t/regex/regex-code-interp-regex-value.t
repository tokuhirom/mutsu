use Test;

plan 4;

my $r = rx/(\d)/;
is ("a1" ~~ / a $( $r ) /).Str, 'a1', '$( $re ) matches a Regex value as a pattern';
is ("a1" ~~ / a <$r> /).Str, 'a1', '<$re> agrees';
ok ("a." !~~ / a $( $r ) /), 'the Regex is not matched as literal text';
my $s = 'a.';
ok ("a." ~~ / $( $s ) /), 'a Str value is still matched literally';
