use Test;

use lib 't/lib';
use WithTailHelper;

plan 1;

my $text = qq:to/END/;
value { tail-helper-pos("ok") }
END

is $text.trim, 'value helper-pos(ok)',
    'a heredoc interpolation keeps the value of an imported routine call';
