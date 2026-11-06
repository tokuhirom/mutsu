# Grammar::PrettyErrors: `parse` calls `new` on the instance, `^add_method` of a regex refreshes lookups

Two gaps stopped Grammar::PrettyErrors (its role wraps `ws` from a `method new` that acts only `if self.defined`):

- A grammar's `parse`/`subparse`/`parsefile` always called its `new` on the type object. Rakudo calls `self.new`, so a parse started on an instance runs `new` with that instance (`self.defined` is True). The start rule's invocant is now built from the receiver.
- `Grammar.^add_method('ws', &regex)` registered the rule but did not bump the method generation, so a `.^find_method('ws')` answered `Mu` before the call stayed `Mu` after it. Both registration paths now bump it.

Grammar::PrettyErrors t/00-t/05 and t/99 now match rakudo (7 of 7 files).
