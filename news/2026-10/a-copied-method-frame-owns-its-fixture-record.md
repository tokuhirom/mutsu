# A copied method frame owns a clone of its fixture record

`try_catch_inline` runs a CATCH handler on a copy of the installing frame, and pushed that `Copy`
with a plain `push` while it still named the original's fixture record (ADR-14633 slice 1). Debug
builds tripped the assertion in `RoutineStack::push`; release builds pushed a second frame naming
one record, so popping either cut the record of the other (#14770).

`RoutineStack::push_copy` now gives the copy its own clone of the record. The rule-def-file frame
switch of regex rules, which popped and re-pushed the top frame, replaces it in place instead
(`replace_top`, which also keeps the lexical-package count right when that field changes).
