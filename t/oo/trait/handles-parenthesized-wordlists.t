use Test;
plan 9;

# Config::BINDish::Grammar uses handles(<keys Bool>) on its InSet map (#14148).
class WordListDelegator {
    has %.data handles(<keys Bool>);
}
my $words = WordListDelegator.new(data => { answer => 42 });
is $words.keys.sort.join(','), 'answer', 'parenthesized word list delegates keys';
ok $words.Bool, 'parenthesized word list delegates Bool';
nok WordListDelegator.new.Bool, 'empty delegated hash stays false';

class MixedDelegator {
    has %.data handles('elems', <keys Bool>);
}
my $mixed = MixedDelegator.new(data => { a => 1, b => 2 });
is $mixed.elems, 2, 'quoted method can precede a word list';
is $mixed.keys.sort.join(','), 'a,b', 'word list after quoted method delegates keys';

class MultipleListsDelegator {
    has %.data handles(<keys>, <values>);
}
is MultipleListsDelegator.new(data => { a => 7 }).values.join(','), '7',
    'multiple word lists contribute delegation names';

class UnparenthesizedDelegator {
    has %.data handles <keys Bool>;
}
is UnparenthesizedDelegator.new(data => { b => 2 }).keys.join(','), 'b',
    'the unparenthesized spelling uses the same word-list rule';

class NumericWordDelegator {
    has $.data handles(<1e2>);
}
ok NumericWordDelegator.^can('1e2'), 'an allomorphic word keeps its string method name';

class EmptyWordDelegator {
    has $.data handles(< >);
}
is EmptyWordDelegator.new(data => 23).data, 23,
    'an empty word list preserves ordinary attribute construction';
