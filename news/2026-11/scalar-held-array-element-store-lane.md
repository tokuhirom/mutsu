# Element stores into a scalar-held Array take a fast lane

`$d[$i] = $v` and `$d[$i] := ...` on an Array a `$` scalar holds paid the
whole generic element-store path. The `@a[$i] = $v` lane reads its target
from env, and a scalar's container lives in its own slot (ADR-14632), so that
lane always declined it. A new lane writes the slot's node in place. It
handles an `Int` index on a plain mutable Array with a plain scalar rvalue. It
also handles a bind whose source container already exists (an element, or a
variable that already holds a cell), and a bind of a value with no container
(a code object, an object). `$d[$i] = 1` went from 2.5 µs to 0.3 µs.

Two lookups the store paid on every name were also cut short:

- **Unit lexicals.** The compunit's file-scope-lexical lookup
  (`unit_lexical_slot`, `unit_lexical_container_cell`) now first asks the
  store's name filter whether any bucket holds the name. A free-variable read
  in a module routine no longer walks the package chain for a name no file
  scope declares.
- **`initialized` set.** A store that opens no gap no longer builds an
  all-present array's `initialized` set.

Terminal::ANSIParser builds its byte dispatch tables out of a few thousand such
binds per parser. `make-ansi-parser` x100 went from 1.43 s to about 0.95 s
(rakudo 0.30 s). Binding an element to a Block still leaves the element
assignable (#17455).
