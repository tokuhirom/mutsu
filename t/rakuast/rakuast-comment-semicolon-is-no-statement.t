use Test;

# A `;` inside a comment between statements is no empty statement of `.AST`
# (#14880): rakudo has none, and the extra `Statement::Empty` shifted every
# later statement of a file such as roast/S12-methods/calling_syntax.t. A `;`
# that stands between statements still is one.

plan 5;

use experimental :rakuast;

sub kinds(Str $src) { $src.AST.statements.map(*.^name.subst('RakuAST::', '')).join(' ') }

is kinds("say 1;\n# a;b\nsay 2;\n"), 'Statement::Expression Statement::Expression',
    'a `;` in a line comment';
is kinds("say 1;\n\n# L<S02/Bare identifiers/\"\$x.foo;\">\n\nsay 2;\n"),
    'Statement::Expression Statement::Expression', 'the roast comment that made the ratchet fail';
is kinds("say 1;;\nsay 2;;;\n;\nsay 3; # x;y\n;\n"),
    'Statement::Expression Statement::Empty Statement::Expression Statement::Empty '
    ~ 'Statement::Empty Statement::Empty Statement::Expression Statement::Empty',
    'real empty statements are kept, a `;` in a trailing comment is not one';
is kinds("say 1; # c\n;\nsay 2;\n"), 'Statement::Expression Statement::Empty Statement::Expression',
    'a lone `;` after a comment is an empty statement';
is $_.statements[1].origin.from, 7, 'its position' given "say 1;\n;\nsay 2;".AST;
