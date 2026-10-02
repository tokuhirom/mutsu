use v6;
use experimental :rakuast;
use Test;

# A name declared in a nested position (an `if` or loop body, a closure, a
# `do` block) resolves at parse time like one declared at the top level, so a
# later bareword use renders as `Type::Simple` / `Term::Name` (ADR-0011).
# The declared-name scan used to stop at the constructs its `_ =>` arm did
# not list, leaving such a bareword unconvertible (ADR-0137 port).
# Each test uses distinct names: `.AST` registers the symbol.
#
# Passes under BOTH mutsu and raku (rakudo 2026.07).

plan 5;

ok Q{if 1 { class NE1 { } }; NE1.new}.AST.gist.contains('RakuAST::Type::Simple.new('),
    'a class declared in an if body';
ok Q{my $f = -> { class NE2 { } }; NE2.new}.AST.gist.contains('RakuAST::Type::Simple.new('),
    'a class declared in a pointy block';
ok Q{for 1 { constant NE3 = 5 }; NE3}.AST.gist.contains('RakuAST::Term::Name.new('),
    'a constant declared in a loop body';
ok Q{my $x = do { class NE4 { } }; NE4.new}.AST.gist.contains('RakuAST::Type::Simple.new('),
    'a class declared in a do block';
ok Q{while 0 { enum NE5 <a b> }; NE5}.AST.gist.contains('RakuAST::Type::Simple.new('),
    'an enum declared in a while body';
