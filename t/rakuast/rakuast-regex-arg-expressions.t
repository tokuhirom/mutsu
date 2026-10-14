use v6;
use experimental :rakuast;
use Test;

# Subrule arguments that are postfix updates, assignments, block calls or
# block-literal hashes survive the RakuAST round trip: the argument source the
# regex tree keeps is re-spelled from the lowered expression.

plan 8;

sub regex-of(Str $src) { $src.AST.statements[*-1].expression }

ok regex-of(Q|my $c; my regex x ($n) { a }; / <x($c++)> /|).gist.contains('RakuAST::ApplyPostfix.new('),
    'postfix: argument is an ApplyPostfix';
ok regex-of(Q|my regex x ($n) { a }; / <x({ 2 }())> /|).gist.contains('RakuAST::Regex::Assertion::Named::Args.new('),
    'block call: named assertion with args';
ok regex-of(Q|my regex x ($n) { a }; / <x: { key => 1 }> /|).gist.contains('RakuAST::Circumfix::HashComposer.new('),
    'hash block: argument is a HashComposer';
ok regex-of(Q|my $s; my regex x ($n) { a }; / <x($s = 7)> /|).gist.contains('RakuAST::ApplyInfix.new('),
    'assignment: argument is an ApplyInfix';

# --- the EVAL direction computes what the parsed program computes ---------
is EVAL(Q|my $c = 0; my regex x ($n) { a }; "aa" ~~ / <x($c++)> <x($c++)> /; $c|.AST), 2,
    'each postfix argument is evaluated once per subrule call';

is EVAL(Q|my $seen; my regex y ($n) { a { $seen = $n } }; "a" ~~ / <y($seen = 7)> /; $seen|.AST), 7,
    'an assignment argument updates the lexical';

is EVAL(Q|my $got; my regex z ($p) { f { $got = $p.raku } }; "f" ~~ / <z: { key => <v a l> }> /; $got|.AST),
    '${:key($("v", "a", "l"))}', 'a block-literal argument carries a word list';

is EVAL(Q|my $cnt = 0; my regex w ($n) { a }; "a" ~~ / <w({ $cnt++; 2 }())> /; $cnt|.AST), 1,
    'a block-call argument runs once';
