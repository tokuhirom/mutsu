# A Match answers Any's list methods on its positional captures

Rakudo declares `min`, `max`, `minmax`, `sort`, `unique`, `combinations`, `Set`, `Array`, `grep` and the rest of
`Any`'s list methods as `self.list.METHOD`, and a `Match`'s `.list` is its positional captures. mutsu answered
several of them with the whole match as one element (`$m.max` was the match itself, a grammar match's `.min` was
the match instead of `Inf`). Both native dispatch entries now swap such a receiver for its positional list before
anything answers the call, unless the grammar declares the method itself; `.Hash` reads the named captures. The
name-matching `Match` arms of the zero-argument cascade are gone with it (ADR-11276 §9.98, #13161).
