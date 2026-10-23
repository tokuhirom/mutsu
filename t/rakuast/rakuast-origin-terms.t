use Test;

# Terms carry the origin the parser read them from (ADR-10723 Stage 2,
# expression families). Ranges measured on rakudo 2026.09.

plan 16;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }

my $src = q:to/END/;
my @p = (
  $*PROGRAM,
  1.abs,
);
say 42, "x";
END
my @s = $src.AST.statements;

# --- read direction ------------------------------------------------------------
my $paren = @s[0].expression.initializer.expression;
is range($paren), '8..33', 'a parenthesized list';
my @items = $paren.semilist.statements[0].expression.operands;
is range(@items[0]), '12..21', 'a dynamic variable on its own line';
is range(@items[1]), '25..30', 'a method call on a literal';
is range(@items[1].operand), '25..26', 'the literal it is called on';
my @args = @s[1].expression.args.args;
is range(@args[0]), '39..41', 'an integer argument';
is range(@args[1]), '43..46', 'a string argument';

{
    my @lines;
    sub walk($node) {
        @lines.push: .source.original-line(.from) with $node.origin;
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    is @lines.sort.squish.join(','), '1,2,3,5',
        'every line that starts a term reports it';
}

# --- a term the parser imports a meaning for still gets its origin ------------
{
    my @a = q:to/END/.AST.statements;
    use Test;
    is-deeply
      1,
      2;
    END
    my @args = @a[1].expression.args.args;
    is range(@args[0]), '22..23', 'the first argument of an imported listop';
    is @args[1].origin.source.original-line(@args[1].origin.from), 4,
        'the second argument is on its own line';
}

# --- equal terms in one statement, and a closure's own statement ------------
{
    my @a = q:to/END/.AST.statements;
    my $x = 1;
    say $x + $x, { $x }, $x;
    END
    my @args = @a[1].expression.args.args;
    is (range(@args[0].left), range(@args[0].right)).join(' '), '15..17 20..22',
        'two equal variables in one statement take their own ranges';
    is range(@args[1].body.statement-list.statements[0].expression), '26..28',
        'a variable in a closure takes the range in the closure';
    is range(@args[2]), '32..34', 'the same variable after the closure';
}

# --- terms the parser keeps a spelling for -------------------------------------
{
    my @a = q:to/END/.AST.statements;
    my $h = q:to/STOP/;
    abc
    STOP
    say <a b>;
    END
    is range(@a[0].expression.initializer.expression), '8..18', 'a heredoc';
    is range(@a[1].expression.args.args[0]), '33..38', 'a word list';
}

# --- a hand-built or evaluated tree is unaffected --------------------------
is EVAL(q[my @p = (1, 2.abs); @p.sum].AST), 3, 'the round trip still runs';
nok RakuAST::IntLiteral.new(5).origin.defined, 'a hand-built node has no origin';
