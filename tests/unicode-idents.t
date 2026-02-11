use Test;
plan 6;

my $über = 42;
is $über, 42, 'Unicode variable with umlaut';

sub grüß($x) { $x * 2 };
is grüß(3), 6, 'Unicode function name with umlaut and ß';

my $café = "latte";
is $café, "latte", 'Unicode variable with accent';

my $日本語 = "japanese";
is $日本語, "japanese", 'CJK variable name';

my $møøse = 99;
is $møøse, 99, 'Nordic characters in variable name';

sub 加算($a, $b) { $a + $b };
is 加算(1, 2), 3, 'CJK function name';
