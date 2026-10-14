use Test;

# A `#|` / `#=` comment documenting an anonymous sub or block is printed by
# rakudo as a trailing `.declarator-docs(...)` call on the node, and survives
# EVAL as the code object's `.WHY`.

plan 5;

my $src = "my \$f = #| lead\n    anon sub \{\};\n\$f.WHY.Str";
my $ast = $src.AST;
ok $ast.raku.contains(').declarator-docs('), 'the docs are printed after the node';
ok $ast.raku.contains('leading => ('), 'with a leading list';
is $ast.EVAL, 'lead', 'EVAL keeps the leading doc on the anon sub';

my $blk = "my \$b = #| blk\n\{;\n\};\n\$b.WHY.Str".AST;
is $blk.EVAL, 'blk', 'a documented block keeps its doc';

nok "my \$p = anon sub \{\};\n\$p.WHY.defined".AST.EVAL, 'an undocumented anon sub has no doc';
