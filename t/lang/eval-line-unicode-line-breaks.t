use Test;

plan 9;

# `$?LINE` counts every line break rakudo does: \n, \r, \r\n (once), VT, FF,
# U+0085, U+2028 and U+2029.
for ("\x[2028]", "\x[2029]", "\x[85]", "\r", "\r\n", "\x[B]", "\x[C]", "\n") -> $sep {
    my $code = "say \$?LINE;{$sep}say \$?LINE;";
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
        EVAL $code;
    }
    is $out.lines.join(','), '1,2', "\$?LINE after {$sep.ords.map(*.fmt('U+%04X')).join(' ')}";
}

my $s = "say 1;\x[2028]say 2;\x[85]say 3;\rsay 4;\r\nsay 5;";
is $s.AST.statements.map({ .origin.source.original-line(.origin.from) }).join(' '), '1 2 3 4 5',
    'original-line of each statement';
