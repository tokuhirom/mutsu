# usage: <raku|mutsu> dump-deparse.raku FILE
# The `.DEPARSE` text of each top-level statement of FILE (the parity corpus
# of #15792); `@@@ N` lines delimit the statements as `dump.raku` does.
sub MAIN(Str $file) {
    my $ast = slurp($file).AST;
    for $ast.statements.kv -> $i, $s {
        say "@@@ $i";
        say $s.DEPARSE;
    }
}
