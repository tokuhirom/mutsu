# A `nextcallee` method candidate runs in its own compunit

The callable `nextcallee` hands out for a method candidate now carries the
candidate's own compiled body and source file. Before, the value call compiled
the method's AST body on the fly in the unit of whoever called `nextcallee`, so
a routine the candidate's module imported was not visible there: a role mixed
in from one module that deferred with `nextcallee` to a class from another died
with `Unknown function`. Red 0.2.17's `Red::Transaction::Guard` does exactly
that (its `ScopedDriver.prepare` reaches `Red::Driver.prepare`, which calls the
imported `protected-row-query`), and every Red test that created a table died
there (#19172).
