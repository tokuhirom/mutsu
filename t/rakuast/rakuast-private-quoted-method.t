use Test;

plan 6;

my $text = Q[class PrivQuotedText { method m($n) { self!"$n"() }; method !secret { 'hidden' } }].AST.raku;
ok $text.contains('RakuAST::Call::QuotedMethod.new('), 'self!"$n"() is a Call::QuotedMethod';
ok $text.contains('dispatch => "!"'), 'the private dispatch string is a bare !';
ok $text.contains('RakuAST::Term::Self.new'), 'the invocant is Term::Self';
ok Q[class PrivQuotedLit { method m { self!"secret"() }; method !secret { 'lit' } }].AST.raku.contains('dispatch => "!"'),
    'a literal quoted private name keeps the dispatch string';

# EVAL uses names the textual parse above never saw: both declare the class.
EVAL Q[class PrivQuotedA { method m($n) { self!"$n"() }; method !secret { 'hidden' } }].AST;
is ::('PrivQuotedA').new.m('secret'), 'hidden', 'EVAL of the round-tripped private quoted call';
EVAL Q[class PrivQuotedB { method m { self!"secret"() }; method !secret { 'lit' } }].AST;
is ::('PrivQuotedB').new.m, 'lit', 'EVAL of a literal quoted private call';
