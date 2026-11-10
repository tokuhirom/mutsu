# `.roll($count)` takes a Rat count

`(^20).list.roll(10/2)` died with `No such method 'roll' for invocant of type 'List'`: `roll_n`
turned an Int, Num or Str count into a number and declined everything else, so a Rat fell
through to "no such method". `.pick` already accepted a Rat; `roll` now truncates it the same
way, and an infinite Rat (`1/0`) means `*` (ParaSeq's `.roll($elems / 2)`, `t/26-roll.t`).
