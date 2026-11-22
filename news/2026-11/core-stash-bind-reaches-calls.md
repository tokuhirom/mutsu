# A routine bound into CORE:: is what a call of the core name reaches

`&CORE::none := &f` (or `CORE::<&none> := &f`) already changed what `&none` and
`&CORE::none` read. A call `none(1, 2)` still went to the builtin junction constructor,
because the call path never looked at the setting's stash. Now, when a program declares
no routine of its own by that name, the call reaches the bound routine, as it does with
rakudo's one shared CORE stash. The `our` store records the names bound this way, so a
call of any other name pays one set probe. The first bind of a name also moves the
registry write generation, so a call site that had already learned the builtin route
re-resolves (#18779).
