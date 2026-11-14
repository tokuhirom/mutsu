# Fixture for t/rakuast/rakuast-nested-multi-compile-time-value.t: a module
# whose code walks the importing unit's `$*CU` at CHECK time, as Red's
# `red-compile` does, and a routine the unit imports.
unit module RakuastCheckWalker;
sub helper-value($x) is export { $x * 2 }
sub plus-candidate-files($cu = $*CU) is export {
    my @files;
    sub walk($n) {
        if $n ~~ ::('RakuAST::Infix') && $n.operator eq '+' && $n.is-resolved {
            my $code = $n.resolution.compile-time-value;
            @files.push: ($code.^can('candidates') ?? $code.candidates !! ($code,)).map(*.file).List;
        }
        $n.visit-children(&walk);
    }
    walk($cu);
    @files
}
