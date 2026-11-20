# `@!x.push` and method calls on `@!x` / `%!x` go through an attribute slot

A scalar attribute `$!x` has long been addressed through a local slot that reads `self`'s attribute cell through a per-site cache (ADR-0121). The aggregate forms were not. `@!lines.push($l)`, `@!lines.elems` and `%!seen.keys` compiled by name, so each one probed the env by name, snapshotted the attribute's container out of the cell, ran the generic by-name container op on that copy, and mirrored the result back into the cell.

Those forms now compile to an attribute slot (ADR-18356 D2). A read is a `GetLocal` of the slot, which reads the cell through the site cache. `push` and a method call take the slot forms of `ArrayPush` and `CallMethodMut`. Around those ops the VM refreshes the slot from the cell and writes the cell back only if the op rebound the slot instead of mutating the container in place. The slot is a reference to the container the instance holds, never a copy. Element stores (`%!h<k> = v`, `@!a[$i]++`) still compile by name; they are the next step.

Per operation, callgrind slope on a profiling build:

| | before | after |
| --- | ---: | ---: |
| `@!lines.push('a')` | 20.9k Ir | 5.8k Ir |
| `$s = @!lines.elems` | 13.0k Ir | 8.3k Ir |

`@!lines.push` now costs what a local `@lines.push` does (5.9k). MCP::Server::Tool::Web's `html-to-text` slope drops by 2.1%: its renderer pushes every finished line into `@!lines` and `@!verbatim-lines`.
