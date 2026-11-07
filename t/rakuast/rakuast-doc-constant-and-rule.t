use Test;
use experimental :rakuast;

plan 6;

sub docs-of(Str $src) {
    my $node = $src.AST.statements[0].expression;
    $node.raku.lines.grep(/leading|trailing/).join('|').subst(/\s+/, ' ', :g)
}

# `#|` on a constant, and on a rule/token declared outside a grammar, are
# declarator docs of that declaration (they were dropped).
is docs-of("#| c\nconstant K = 3;"), ' leading => (', 'a documented constant';
is docs-of("#| u\nrule r \{ x }"), ' leading => (', 'a documented rule';
is docs-of("#| t\ntoken tk \{ y }"), ' leading => (', 'a documented token';

my sub names($node) {
    my @n;
    $node.visit-children({ @n.push(.^name.split('::').tail) });
    @n
}
my $c = "#| c\nconstant K = 3;".AST.statements[0].expression;
is names($c).tail, 'Declarator', 'a constant visits its Doc::Declarator last';
is $c.DEPARSE, "#| c\nconstant K = 3", 'the constant deparses with its doc';
is "constant K = 3;".AST.statements[0].expression.raku.contains('declarator-docs'), False,
    'an undocumented constant has no docs';
