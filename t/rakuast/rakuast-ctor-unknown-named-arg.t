use Test;

# #16771: a RakuAST named constructor refuses a named argument the class does
# not take, with the bind's own message, as rakudo does. mutsu accepted it and
# ignored it. A name rakudo accepts but mutsu does not keep (`For`'s `otherwise`,
# which rakudo drops too, and `Subset`'s `WHY`) is still accepted.

plan 18;

sub s($v) { RakuAST::StrLiteral.new($v) }
my $blk = RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new));
my $src = RakuAST::Var::Lexical.new('$x');

sub refused(&code, $what) {
    my $msg = '';
    try { code(); CATCH { default { $msg = .message } } }
    is $msg.lines.head, "Unexpected named argument 'bogus' passed", $what;
}

refused { RakuAST::Doc::Block.new(type => 'para', bogus => 1) }, 'Doc::Block refuses an unknown name';
refused { RakuAST::Doc::Markup.new(letter => 'B', bogus => 1) }, 'Doc::Markup';
refused { RakuAST::Ternary.new(condition => $src, then => $src, else => $src, bogus => 1) }, 'Ternary';
refused { RakuAST::FatArrow.new(key => 'k', value => $src, bogus => 1) }, 'FatArrow';
refused { RakuAST::Statement::For.new(source => $src, body => $blk, bogus => 1) }, 'Statement::For';
refused { RakuAST::Statement::Given.new(source => $src, body => $blk, bogus => 1) }, 'Statement::Given';
refused { RakuAST::Statement::Catch.new(body => $blk, bogus => 1) }, 'Statement::Catch';
refused { RakuAST::Type::Subset.new(name => RakuAST::Name.from-identifier('S'), bogus => 1) }, 'Type::Subset';
refused { RakuAST::Doc::Declarator.new(WHEREFORE => $src, bogus => 1) }, 'Doc::Declarator';
refused { RakuAST::Call::Term.new(bogus => 1) }, 'Call::Term';

# the first unknown name is the one reported, wherever it is in the list
my $msg = '';
try { RakuAST::Doc::Block.new(zzz => 1, type => 'para', bogus => 1); CATCH { default { $msg = .message } } }
is $msg.lines.head, "Unexpected named argument 'zzz' passed", 'the first unknown name is reported';

# every name the class takes is still accepted
lives-ok { RakuAST::Doc::Block.new(margin => ' ', type => 'para', level => 2, config => {a => 1}, paragraphs => (),
    directive => False, for => False, abbreviated => False) }, 'all of Doc::Block\'s names';
lives-ok { RakuAST::Statement::For.new(source => $src, body => $blk, mode => 'serial') }, 'Statement::For\'s mode';
lives-ok { RakuAST::Type::Subset.new(name => RakuAST::Name.from-identifier('S'), where => $src, traits => ()) },
    'Type::Subset\'s where and traits';

# accepted by rakudo although not kept
lives-ok { RakuAST::Statement::For.new(source => $src, body => $blk, otherwise => $blk) },
    'For accepts otherwise, as rakudo (which drops it)';
ok !RakuAST::Statement::For.new(source => $src, body => $blk, otherwise => $blk).raku.contains('otherwise'),
    'and does not print it';
lives-ok { RakuAST::Type::Subset.new(name => RakuAST::Name.from-identifier('S'),
    WHY => RakuAST::Doc::Declarator.new(WHEREFORE => $src, leading => ('x',))) },
    'Subset accepts WHY';

# a name given once, in any order, is unaffected
is RakuAST::Doc::Block.new(paragraphs => (), type => 'code').type, 'code', 'order does not matter';
