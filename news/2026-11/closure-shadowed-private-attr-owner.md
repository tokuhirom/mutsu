# A closure reads its own class's copy of a shadowed private attribute

When a class and its parent both declare `$!x`, each keeps its own copy, and `$!x` names the
copy of the class the code is written in. A method found that class on the dispatch stack, but a
closure the parent's method made runs in a frame of its own: after the method returned, inside
it, or called from a child's method, the dispatch stack named another class or none, and the
access fell back to the most-derived copy -- `B.new.mk()()` read B's `$!x` where Rakudo reads A's
(#17936). An attribute access in a closure or block now takes its owner from the fixture record
of the method it was created in (`?ROLE`, else `?CLASS`, ADR-14843), and the container a closure
returns for such an attribute is the owner's copy too. The same bug for two roles mixed into one
object depends on how a reblessed mixin type stores role attributes (#17890) and remains open.
