# Bounded type-check recursion and `Array[T]()` from a Seq

`type_matches_value_why` recurses through subset bases and parameterizations without crossing a Raku
call boundary, so the VM's stack guard never saw a cycle and the process aborted with a native stack
overflow (Physics::Unit). It now consults the native-stack floor on entry and refuses the check with
the catchable "Too deep recursion" exception when the thread is out of headroom.

`Array[T]()` as a parameter, and `--> Names()` for `subset Names of Array[Name]`, now build the typed
array from a Seq, as `Array[T].new(|$seq)` does, instead of dying with "Impossible coercion from
'Seq'".
