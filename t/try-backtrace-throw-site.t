use Test;
plan 2;

try {
    my $unused = 1;
    die 'boom';
}

is $!.backtrace.list[0].line, 6,
    'try preserves the die line in the structured backtrace';
like $!.backtrace.Str, /'line 6'/,
    'try preserves the die line in the rendered backtrace';
