# Threads mixing a role into a shared type no longer get a half-composed type

The type registry is one world shared by every thread (ADR-17216), and a mixin
type `C+{R}` is written into it in several steps: the class first, then the
role's methods. Threads that asked for the same mixin type at the same moment
(`$pun but Marker` and then `.new`, from a type cached in a shared `Map`) could
find the class registered before its methods were, and died with
`No such method 'mark' for invocant of type 'Conc::Maker[Any]+{Marker}'`
(#18266; it reddened unrelated PRs' CI about once in six runs).

Composing a mixin type now holds a process-wide, per-thread re-entrant lock
from the "is it already registered?" check to its publication, so a thread
either composes the whole type or waits for the one that does, as rakudo's
metamodel does.
