use v6;
use experimental :rakuast;
use Test;

# `$(...)`, `@(...)` and `%(...)` across the RakuAST boundary. Measured against
# rakudo 2026.09, each is a `Contextualizer::*` over a `StatementSequence`, not
# a `.item` / `.list` / `.hash` method call, and a user-written call of the same
# name stays a call.

plan 19;

sub node-of(Str $src) { $src.AST.statements[0].expression }

is node-of(Q|$(1, 2)|).^name, 'RakuAST::Contextualizer::Item', '$(...) is Contextualizer::Item';
is node-of(Q|@(1, 2)|).^name, 'RakuAST::Contextualizer::List', '@(...) is Contextualizer::List';
is node-of(Q|%(1, 2)|).^name, 'RakuAST::Contextualizer::Hash',
    '%(...) of non-pair contents is Contextualizer::Hash';

for Q|$(1, 2)|, Q|@(1, 2)|, Q|%(1, 2)| -> $src {
    my $gist = $src.AST.gist;
    ok $gist.contains('RakuAST::StatementSequence.new(')
        && $gist.contains('RakuAST::ApplyListInfix.new(')
        && !$gist.contains('Call::Method'),
        "$src holds a StatementSequence and no method call";
}

# --- the itemized forms: the inner contextualizer is the child itself --------
my $il = node-of(Q|$@(1, 2)|);
is $il.^name, 'RakuAST::Contextualizer::Item', '$@(...) is an Item';
my $ih = node-of(Q|$%(1, 2)|);
is $ih.^name, 'RakuAST::Contextualizer::Item', '$%(...) is an Item';
my $nested = Q|$@(1, 2)|.AST.gist;
ok $nested.contains('RakuAST::Contextualizer::Item.new(')
    && $nested.contains('RakuAST::Contextualizer::List.new(')
    && !$nested.contains('Call::Method'),
    '$@(...) nests List directly in Item';

# `$(@(...))` is a different tree: the Item holds a StatementSequence.
my $wrapped = Q|$(@(1, 2))|.AST.gist;
ok $wrapped.index('Contextualizer::Item.new(') < $wrapped.index('RakuAST::StatementSequence.new(')
    < $wrapped.index('Contextualizer::List.new('),
    '$(@(...)) holds the List inside a StatementSequence';

# --- a user-written call keeps rendering as a call ---------------------------
my $call = Q|(1, 2).list|.AST.gist;
ok $call.contains('Call::Method') && !$call.contains('Contextualizer'),
    'a written .list call is still a method call';

# --- the empty contextualizer ------------------------------------------------
ok Q|@()|.AST.gist.contains('Contextualizer::List.new(')
    && Q|@()|.AST.gist.contains('RakuAST::StatementSequence.new()'),
    '@() is a List over an empty StatementSequence';

# --- literal-keyed %(...) is still the hash literal ---------------------------
is node-of(Q|%(a => 1)|).^name, 'RakuAST::Contextualizer::Hash', '%(a => 1) is Contextualizer::Hash';

# --- the write direction -----------------------------------------------------
is-deeply EVAL(Q|@(1, 2)|.AST), (1, 2), 'EVAL of @(...) round-trips';
is-deeply EVAL(Q|%(1, 2)|.AST), {"1" => 2}, 'EVAL of %(non-pairs) round-trips';
is EVAL(Q|$@(1, 2)|.AST).raku, '$(1, 2)', 'EVAL of $@(...) itemizes the list';
is-deeply EVAL(Q|@(1, 2).map(* + 1)|.AST).List, (2, 3), 'a contextualizer composes with a method call';

# --- semantics: the compiled node still behaves like the call ----------------
my @a = 1, 2;
is-deeply @(@a, 3).elems, 2, '@(...) still builds a List';
is $(1, 2).raku, '$(1, 2)', '$(...) still itemizes';
