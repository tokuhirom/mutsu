use Test;

# #18831: private methods are not inherited or overridden. An unqualified
# `self!m` names the `!m` of the class the calling method is written in (or of
# the class a role's method was composed into), never a same-named private
# method a subclass of the receiver declares.

plan 9;

class Parent {
    method !secret(Int $x) { "parent $x" }
    method reveal(Int $x) { self!secret($x) }
}
class Child is Parent {
    method !secret(Int $x) { "child $x" }
    method own(Int $x) { self!secret($x) }
}
is Child.new.reveal(1), 'parent 1', "a parent's method calls the parent's private method";
is Child.new.own(1), 'child 1', "a child's method calls the child's private method";
is Child.new.reveal(2), 'parent 2', 'a second call answers the same (no stale cache)';

class Q { method !z { 'Q' }; method go { self!z } }
class Q2 is Q { method !z { 'Q2' } }
is Q2.new.go, 'Q', 'zero-argument private method in a parent method';

role R { method !who() { 'role' }; method from-role() { self!who } }
class P does R { method !who() { 'P' }; method from-p() { self!who } }
class C is P { method !who() { 'C' } }
is C.new.from-p, 'P', "a class's method ignores a subclass's private method";
is C.new.from-role, 'P', "a role's method calls the composing class's private method";
is P.new.from-role, 'P', "the composing class's own private method overrides the role's";

role R2 { method !w { 'R2' }; method fr { self!w } }
class P2 does R2 { }
class C2 is P2 { method !w { 'C2' } }
is C2.new.fr, 'R2', "a role's private method is not overridden by a subclass of its composer";
class D does R2 { method !w { 'D' } }
is D.new.fr, 'D', 'a class that composes the role directly overrides its private method';
