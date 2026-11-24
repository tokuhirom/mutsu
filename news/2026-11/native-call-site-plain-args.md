# A builtin call with plain scalar arguments skips the special-case cascade

#18102 taught a `uniname($_)`-style call site to remember that a builtin
answers it. A remembered call still went through `try_native_function`, whose
special cases test the name and the arguments in turn: gather and shaped
arguments, atomic and native-reference routines, `zip`, the free
`floor`/`ceiling`, the type-object gate of the string routines, deferred
`.map` Seqs, Instances, `Map`, and lazy lists.

None of these cases applies when every argument is a defined Int, Num, Str,
Bool or Rat and the name is not one of the six with an arm of their own. The
site now records that when it first remembers the route, and a matching call
goes straight to the builtin.

`uniname` also spells `<reserved-0378>`-style sentinels and derived ideograph
names (`CJK UNIFIED IDEOGRAPH-4E00`) with a hand-written hex writer instead of
`format!`, which cost ~700 instructions a call. Most of the codepoint range
takes one of these two forms.

On a release build, an iteration of `for 0..0x10FFFF { $c++ if uniname($_) }`
drops from 8.3k to 7.5k instructions over named codepoints, and from 7.8k to
6.8k over unassigned ones. The full loop takes about 1.35x rakudo's time
(#18028).
