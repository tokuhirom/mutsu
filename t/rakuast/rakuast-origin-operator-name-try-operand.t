use Test;
use experimental :rakuast;

# Origins of an operator routine name's word quote (`term:<forty-two>`) and of the
# statement a `try EXPR` holds. Offsets measured on Rakudo 2026.11 (#15780).

plan 12;

sub find(RakuAST::Node $root, Str $class) {
    my @found;
    sub walk($n) { @found.push($n) if $n.^name eq $class; $n.visit-children(&walk) }
    walk($root);
    @found
}

my $src = 'sub term:<forty-two> { 42 }';
my $sub = $src.AST;
my ($quote) = find($sub, 'RakuAST::QuotedString');
is $quote.origin.from, 9, 'the quote starts at its <';
is $quote.origin.to, 20, 'and ends after its >';
my ($text) = find($sub, 'RakuAST::StrLiteral');
is $text.origin.from, 10, 'the literal starts after the <';
is $text.origin.to, 19, 'and ends before the >';

my $trait = 'multi trait_mod:<is>(Variable:D $v, :$x) { }'.AST;
my ($tq) = find($trait, 'RakuAST::QuotedString');
my $trait-src = 'multi trait_mod:<is>(Variable:D $v, :$x) { }';
is $tq.origin.from, $trait-src.index('<is>'), 'the quote of a trait_mod name starts at its <';
is $tq.origin.to, $trait-src.index('<is>') + 4, 'and ends after its >';

my $try = "my \$y = 1;\nmy \$x = try \$y.foo;\n".AST;
my ($prefix) = find($try, 'RakuAST::StatementPrefix::Try');
my @inner = find($prefix, 'RakuAST::Statement::Expression');
is @inner.elems, 1, 'a try holds one statement';
is @inner[0].origin.from, 23, 'which starts at the expression';
is @inner[0].origin.to, 29, 'and ends with it';
is @inner[0].origin.to - @inner[0].origin.from, '$y.foo'.chars, 'its length is the expression\'s';

my $do = 'my $z = do 1 + 2;'.AST;
my ($d) = find($do, 'RakuAST::StatementPrefix::Do');
my @s = find($d, 'RakuAST::Statement::Expression');
my $do-src = 'my $z = do 1 + 2;';
is @s[0].origin.from, $do-src.index('1 + 2'), 'the same for do EXPR (start)';
is @s[0].origin.to, $do-src.index('1 + 2') + 5, 'and end';
