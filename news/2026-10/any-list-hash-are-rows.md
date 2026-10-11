# `Any.list` and `Any.hash` are method-table rows

The scalar and fall-through readings of `.list` and `.hash` go through two `Any` rows whose handlers the dispatch cascade also calls
(ADR-11276 §9.65). No behaviour change.
