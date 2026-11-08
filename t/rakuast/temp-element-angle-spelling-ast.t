use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 7;

# `temp %h<a>` keeps its angle-bracket subscript in the tree (#14845).
for 'temp %h<a> = 5', 'temp %h<a>', 'let %h<a> = 5', 'temp %h{"a"} = 5', 'temp @a[0] = 1' -> $src {
    my $decl = $src.starts-with('temp @') ?? 'my @a; ' !! 'my %h; ';
    is ($decl ~ $src).AST.DEPARSE.trim.lines[1], $src, "DEPARSE of '$src'";
}
is EVAL('my %h = a => 1; { temp %h<a> = 9; }; %h<a>'.AST), 1, 'temp restores on scope exit (AST)';
is EVAL('my %h = a => 1; { temp %h<a> = 9; }; %h<a>'), 1, 'temp restores on scope exit (string)';
