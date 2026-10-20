# A block that declares a routine no longer walks every `our` sub on exit

Leaving a block that declares a routine rolls the routine table back, then puts
back each `our sub` the rolled-back table lacks. Finding those meant walking
the whole our-scoped table on every exit: O(o), o = `our` subs registered. So
`for ^N { { sub g { 1 }; g() } }` in a program with 4000 `our` subs spent
188K instructions an iteration on that walk alone (#9170).

When the block left the our-scoped table alone, the answer depends only on
that table, the rolled-back map and the current package. A loop body asks the
same question on every iteration, so the restore now keeps the last answer and
reuses it while all three match. The table is held by the memo and the map is
named by its version stamp, so a match can only mean the same content.
