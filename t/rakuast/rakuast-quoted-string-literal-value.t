use experimental :rakuast;
use Test;

# `RakuAST::QuotedString.literal-value` (and `maybe-compile-time-value`): the
# value a quote denotes when it has no interpolation and no run-time processor
# (#17922), as rakudo's RakuAST frontend answers it.

plan 20;

sub quote($src) { $src.AST.statements[*-1].expression.args.args[0] }

is quote('say "abc"').literal-value, 'abc', 'a plain double-quoted string';
is quote(q{say 'x'}).literal-value, 'x', 'a single-quoted string';
is quote('say ""').literal-value, '', 'an empty string';
is quote('say qq{a\tb}').literal-value, "a\tb", 'escapes are processed';
is quote('say Q{a\nb}').literal-value, 'a\nb', 'Q keeps a backslash';
is quote("say q:to/E/;\n  hi\n  E\n").literal-value, "hi\n", 'a heredoc, dedented';
is-deeply quote('my $x; say "a$x"').literal-value, Nil, 'an interpolated variable has none';
is-deeply quote('say "a{1}b"').literal-value, Nil, 'an interpolated block has none';

is-deeply quote('say qw/a b/').literal-value, ('a', 'b'), 'words';
is-deeply quote('say qw/a/').literal-value, ('a',), 'words of one word stay a list';
is-deeply quote('say < >').literal-value, (), 'no words';
is-deeply quote('say <a 42>').literal-value, ('a', IntStr.new(42, '42')),
    'val makes allomorphs of the words';
is-deeply quote('say <42>').literal-value, IntStr.new(42, '42'),
    'a < > quote of one word is that word';
is-deeply quote('say <a>').literal-value, 'a', 'also when it is no number';
is-deeply quote('say «a "b c" d»').literal-value, ('a', 'b c', 'd'),
    'quotewords keeps a quoted atom as one word';
is quote('say <a 42>').literal-value(:stringify), 'a 42',
    ':stringify answers the text before the word processors';

is-deeply quote('say <a 42>').maybe-compile-time-value, ('a', IntStr.new(42, '42')),
    'maybe-compile-time-value answers the same';

is RakuAST::QuotedString.new(
    segments => (RakuAST::StrLiteral.new('x'), RakuAST::StrLiteral.new('y'))
).literal-value, 'xy', 'a hand-built quote joins its literal segments';
is-deeply RakuAST::QuotedString.new(
    segments => (RakuAST::StrLiteral.new('x 42'),), processors => <words val>
).literal-value, ('x', '42'), 'a hand-built quote keeps its words Strs';
is-deeply RakuAST::QuotedString.new(segments => (RakuAST::IntLiteral.new(4),)).literal-value,
    Nil, 'a non-string segment has none';
