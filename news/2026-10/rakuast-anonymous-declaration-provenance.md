# Anonymous declarations retain their provenance

Named variables such as `$__ANON_STATE__` and `$__ANON_STATE_9000__` now
remain named declarations across the RakuAST boundary. Their initializers
are retained, and a named `state` declaration is no longer mistaken for an
implicit declaration generated for a bare `$`.

The parser and RakuAST lowering record anonymous declaration provenance
directly, and declaration rewrites preserve it. Actual anonymous variables
keep their existing state behavior. Internal scalar names now include a
character that cannot occur in a source identifier, so VM state storage and
method invocant handling also distinguish them from legal user variables.
A focused regression compares declaration nodes, both frontends, EVAL,
typed assignment, method invocants, and repeated routine calls with Rakudo.

Typed anonymous declarations and their constructors now share the ordinary
declaration lowering for assignment, binding, and call-assignment initializers.
State declaration sites also belong to their compilation session: separate
EVAL units initialize independently, while alternate signatures of one logical
body retain shared state.
