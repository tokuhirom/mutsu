# The lean local push takes class instances

`@a.push($obj)` on a plain declared `@` local took the lean push
(`vm_data_push_lean`) only for an immutable scalar value; pushing an instance
of a class (`@animals.push(Dog.new(...))`) ran the general `ArrayPush`, whose
element-type, native-width, alias and by-name target probes cost ~3,500
instructions per push. The general push hands an instance to `itemize_value`
unchanged, so the lean push now admits one too (not a lazy `Match`, a `Proxy`
or a boot object).

`@a.push($obj)` in a loop: 5,689 → 2,388 instructions per iteration
(profiling build, callgrind).
