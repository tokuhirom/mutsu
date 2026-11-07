# `use Slang &sub` hands the unit's own routines to the slang's `sub EXPORT`

A slang's `use` arguments that name subs of the importing unit (`sub integer {...}; use
overload::constant &integer, &decimal;`, #17055) used to be dropped, so the slang activated
with no arguments. The parser now keeps each plain named sub's declaration text per lexical
scope; slang activation ships it with the `use` arguments and the slang host rebuilds the
routine, so the `EXPORT` hook (and the actions role it refines the grammar with) can call it
at parse time. The sub must not close over other lexicals of the unit.
