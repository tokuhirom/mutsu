# A slang may override the core `statement` action

rakudo runs a slang actions role's `method statement(Mu $/)` for every
statement it parses; Dawa's debugger uses it to wrap each statement in a call
of its stop point. mutsu refused the override at activation ("grammar rule
override 'statement' is not supported"), so `use Dawa` died.

The pre-finalization action pass (ADR-15762 slice 3) now offers every
statement after the activating `use` to the override, top level and nested:
the statement's node, with a `$/` spanning the statement in its unit's text
(`.orig`, `.from`, `.to`) and `$*ORIGIN-SOURCE.original-file` naming the
unit's file. A node the override hands back unchanged leaves the parsed
statement as it is; a replacement is lowered in its place.

Dawa now loads, and its five test files pass.
