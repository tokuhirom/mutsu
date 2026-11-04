use Test;

# The import tag of `use experimental :rakuast` is a `ColonPair::True` with
# the range Rakudo gives it (ADR-10723 Stage 2, #15780). Ranges measured on
# rakudo 2026.09.

plan 2;

sub tag-range(Str $src) {
    my @r;
    sub walk($node) {
        if $node.^name eq 'RakuAST::ColonPair::True' {
            with $node.origin { @r.push: "{.from}..{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(',')
}

is tag-range('use experimental :rakuast;'), '17..25', 'a single import tag';
is tag-range("use experimental :rakuast;\nsay 1;"), '17..25', 'with a following statement';
