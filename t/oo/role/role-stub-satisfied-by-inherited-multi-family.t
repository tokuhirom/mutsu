use Test;
use MONKEY-SEE-NO-EVAL;

# A role's non-multi stub (`method delegate { ... }`) is satisfied by a method
# family the class INHERITS (a `:D:`/`:U:` multi pair), by name, as by a single
# inherited method (PDF::Native's COSType role against COSNode's `_Node`).

plan 4;

role Typed[$cls] {
    method delegate { ... }
}
class Base {
    multi method delegate(::?CLASS:D:) { "defined" }
    multi method delegate(::?CLASS:U:) { "type object" }
}
class Mid is Base { }

my $composed;
lives-ok { EVAL q[class Leaf is Mid { also does Typed[Leaf]; }; $composed = Leaf] }, 'composing the role over an inherited multi family lives';
is $composed.new.delegate, 'defined', 'the :D: candidate is reached';
is $composed.delegate, 'type object', 'the :U: candidate is reached';

role Needs { method must-have { ... } }
throws-like { EVAL q[class Lacking does Needs { }] }, Exception, 'a stub nothing implements still dies';
