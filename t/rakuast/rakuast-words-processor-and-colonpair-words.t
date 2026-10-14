use v6;
use Test;
use experimental :rakuast;

# Two word-quote spellings measured on rakudo 2026.09:
#
# - an interpolating `words` quote (`qqw/a $x b/`, `qq:w:v/$x 1/`) is a
#   `QuotedString` with `processors => ("words",)` (or `<words val>`) over its
#   interpolation segments, split into words at run time;
# - a colonpair whose value is a `«…»` / `<<…>>` word quote
#   (`:args«green yellow»`) is a `ColonPair::Value` over the same
#   `QuotedString(processors => <quotewords val>)` a bare `«…»` term is.

plan 20;

sub expr(Str $source) { $source.AST.statements[*-1].expression }

# --- read direction ---------------------------------------------------------

my $qqw = expr(Q[my $x; qqw/a $x b/]);
is $qqw.^name, 'RakuAST::QuotedString', '`qqw/a $x b/` is a QuotedString';
is-deeply $qqw.processors.List, ('words',), 'with the `words` processor';
is $qqw.segments.elems, 3, 'over its interpolation segments';
is $qqw.segments[1].^name, 'RakuAST::Var::Lexical', 'the variable is a segment of its own';

my $val = expr(Q[my $x; qq:w:v/$x 1/]);
is-deeply $val.processors.List, ('words', 'val'), '`qq:w:v` adds the `val` processor';

my $pair = expr(Q[sub f(*%) { }; f(:args«green yellow»)]).args.args[0];
is $pair.^name, 'RakuAST::ColonPair::Value', '`:args«…»` is a ColonPair::Value';
is $pair.key, 'args', 'with its key';
is $pair.value.^name, 'RakuAST::QuotedString', 'over a QuotedString';
is-deeply $pair.value.processors.List, ('quotewords', 'val'), 'with the quotewords processors';
is $pair.value.segments[0].value, 'green yellow', 'and the raw text';
is expr(Q[sub f(*%) { }; f(:args<<a b>>)]).args.args[0].value.^name, 'RakuAST::QuotedString',
    '`:args<<…>>` likewise';

# --- write direction (EVAL) and semantics -----------------------------------

is-deeply Q[my $x = 'X'; qqw/a $x b/].AST.EVAL, ('a', 'X', 'b'), 'qqw splits the interpolation';
is-deeply Q[my $x = 'p q'; qqw/a $x/].AST.EVAL, ('a', 'p', 'q'),
    'an interpolated value with a space splits too';
is-deeply Q[my $n = 2; qq:w:v/$n 1/].AST.EVAL, (IntStr.new(2, '2'), IntStr.new(1, '1')),
    '`:v` makes allomorphs';
is-deeply Q[my $n = 2; qqw<a $n>].AST.EVAL, ('a', '2'), 'qqw<> interpolates';
is-deeply Q[sub f(*%h) { %h<args> }; f(:args«green yellow»)].AST.EVAL, ('green', 'yellow'),
    'the colonpair passes the word list';
is Q[sub f(*%h) { %h<args> }; f(:args«one»)].AST.EVAL, 'one', 'a one-word colonpair is the word';
is-deeply Q[(:k<< a<b c >>)].AST.EVAL, (k => ('a<b', 'c')), '`:k<<…>>` keeps `<` as word text';

my $x = 'X';
is-deeply qqw/a $x b/, ('a', 'X', 'b'), 'the ordinary frontend agrees on qqw';
is-deeply (:k«a b»), (k => ('a', 'b')), 'and on the colonpair';
