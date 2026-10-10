# `Any.serial`, `Any.batch` and `List.chrs` are method-table rows

The three methods now go through rows whose handlers the dispatch cascade also calls, so each has one implementation (ADR-11276 §9.64).
No behaviour change.
