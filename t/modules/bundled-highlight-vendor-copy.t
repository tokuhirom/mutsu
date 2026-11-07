use Test;

plan 2;

# modules/Rakudo-Core carries the vendored RakuAST::Deparse::Highlight files so
# `use RakuAST::Deparse::Highlight` finds them as a bundled module; they must
# stay byte-identical to vendor/rakudo (docs/vendoring.md).
my $root = $*PROGRAM.absolute.IO.parent(3);
for <Highlight.rakumod Highlight/CORE.rakumod> -> $file {
    is $root.add("modules/Rakudo-Core/lib/RakuAST/Deparse/$file").slurp,
       $root.add("vendor/rakudo/lib/RakuAST/Deparse/$file").slurp,
       "$file is the vendored file";
}
