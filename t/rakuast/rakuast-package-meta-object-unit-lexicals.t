use Test;
use experimental :rakuast;

# A package node's meta-object, read before the unit is EVALed (ADR-17472),
# closes over the unit's earlier lexicals: its methods see what the unit's
# EVAL stores in them (#17522, EEL's `eel -e 'my $kind=...' --eval 'event ...'`).
# Identical under rakudo.
plan 5;

my $ast = Q[my $kind = "ping"; my @list = 1, 2; my %map = a => 1;
            class Plain { method k { $kind }; method l { @list.elems }; method m { %map<a> } };
            sub s { $kind }; &s].AST;
my $type = $ast.find-nodes(RakuAST::Class)[0].meta-object;
is $type.k, Any, 'before EVAL the lexical holds nothing yet';
my &s = $ast.EVAL;
is $type.k, 'ping', 'a method reads a scalar the EVAL assigned';
is $type.l, 2, 'and an array';
is $type.m, 1, 'and a hash';
is s(), 'ping', 'the unit itself reads its own variable';
