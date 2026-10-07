# usage: <raku|mutsu> dump.raku FILE
sub MAIN(Str $file) {
    my $ast = slurp($file).AST;
    for $ast.statements.kv -> $i, $s {
        say "@@@ $i";
        say $s.raku;
    }
}
