use v6;
use Test;

# A heredoc keeps its terminator line in `.AST` (ADR-12199, #12199): rakudo
# writes `q:to/END/` as `Heredoc(segments => (...), stop => "    END\n")`, where
# the parser's own tree is the dedented string and has no record of `stop`.
# This file passes under BOTH mutsu and raku, so raku is the oracle.

plan 43;

sub doc-of(Str $source) {
    $source.AST.statements[*-1].expression;
}

sub texts-of($heredoc) {
    $heredoc.segments.map({ .can('value') ?? .value !! .^name }).join('|');
}

# --- read direction ---
{
    my $h = doc-of("q:to/END/;\n    hi\n    END\n");
    isa-ok $h, RakuAST::Heredoc, 'q:to is a Heredoc';
    is $h.stop, "    END\n", 'stop is the terminator line, indent and newline included';
    is texts-of($h), "hi\n", 'segments hold the dedented text';
}

is doc-of("q:to/END/;\nhi\nEND\n").stop, "END\n", 'an undented terminator';
is texts-of(doc-of("q:to/END/;\nEND\n")), '', 'an empty body is one empty segment';
is doc-of("q:to/END/;\nEND\n").stop, "END\n", 'and its terminator';
is texts-of(doc-of("q:to/END/;\n  a\n b\n END\n")), " a\nb\n", 'dedent by the terminator column';

# --- the same node for every quote form and delimiter ---
for <q qq Q> -> $q {
    for ('/', '/'), ('<', '>'), ('[', ']'), ('"', '"') -> ($open, $close) {
        my $h = doc-of("{$q}:to$open" ~ "END$close;\n a\n END\n");
        is-deeply ($h.^name, $h.stop, texts-of($h)), ('RakuAST::Heredoc', " END\n", "a\n"),
            "{$q}:to$open…$close";
    }
}

is doc-of("q:heredoc/END/;\na\nEND\n").stop, "END\n", ':heredoc';
is texts-of(doc-of("Q:to/END/;\n  a\\n\n END\n")), " a\\n\n", 'Q keeps backslashes';

# --- interpolation ---
{
    my $h = doc-of("my \$x; qq:to/END/;\n    hi \$x \{1+2}\n    END\n");
    isa-ok $h, RakuAST::Heredoc, 'qq heredoc';
    is $h.stop, "    END\n", 'stop';
    is $h.segments.elems, 5, 'literal, variable, literal, block, literal';
    isa-ok $h.segments[1], RakuAST::Var::Lexical, 'the variable';
    isa-ok $h.segments[3], RakuAST::Block, 'the block';
}
is texts-of(doc-of("qq:c:to/END/;\na \{1}\nEND\n")), "a |RakuAST::Block|\n", ':c';

# --- the node inside its surroundings ---
{
    my $call = doc-of("q:to/END/.uc;\n a\n END\n");
    isa-ok $call, RakuAST::ApplyPostfix, 'a postfix call';
    isa-ok $call.operand, RakuAST::Heredoc, 'on a heredoc';
    is $call.operand.stop, " END\n", 'with its stop';
}

{
    my $say = doc-of("say qq:to<E>;\n a\n E\n");
    my $h = $say.args.args[0];
    isa-ok $h, RakuAST::Heredoc, 'the argument of say';
    is $h.stop, " E\n", 'stop';
}

{
    my $infix = doc-of("my \$x = q:to/A/ ~ q:to/B/;\n a\n A\n b\n B\n").initializer.expression;
    isa-ok $infix.left, RakuAST::Heredoc, 'two heredocs on one line: the left';
    isa-ok $infix.right, RakuAST::Heredoc, 'the right';
    is $infix.left.stop, " A\n", 'left stop';
    is $infix.right.stop, " B\n", 'right stop';
    is texts-of($infix.right), "b\n", 'right text';
}

# --- write direction ---
{
    my $h = RakuAST::Heredoc.new(
        segments => (RakuAST::StrLiteral.new("hi\n"),),
        stop     => "    END\n",
    );
    isa-ok $h, RakuAST::Heredoc, 'a hand-built Heredoc';
    is $h.stop, "    END\n", 'keeps its stop';
    is EVAL($h), "hi\n", 'EVAL gives the text';
}

# --- semantics through the round trip ---
is EVAL("my \$x = 5; qq:to/END/;\n  v=\$x\n  END\n".AST), "v=5\n", 'an interpolating heredoc evaluates';
is EVAL("q:to/END/;\n  v=\$x\n  END\n".AST), "v=\$x\n", 'a plain one does not interpolate';
is EVAL("q:to/END/.uc;\n a\n END\n".AST), "A\n", 'a postfix on a heredoc evaluates';
