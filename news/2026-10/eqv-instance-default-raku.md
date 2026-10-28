# `eqv` on same-class instances compares their default `.raku` renderings

Rakudo's `Any:D eqv Any:D` takes the two `.raku` strings, so two objects whose attributes read the same are `eqv` even when an attribute holds two distinct type objects that share a name (`Q.new(v => T1) eqv Q.new(v => T2)` with two generated types both named `Same`). mutsu compared attributes structurally, where distinct type objects are never equal.

Instances of classes without a user `raku` now compare their default renderings, as classes with a user `raku` already did. A plain `List` of type objects still compares its elements (`(T1,) eqv (T2,)` stays false).

Red 0.2.7 `t/72-agg-methods` now passes: `Location.^all.map({ .temps.max(*.temperature) })` built `CASE WHEN ... THEN SELECT ... ELSE SELECT ... END` because its optimizer could not see that the two identical sub-selects (each over its own generated `TempSensor` type) were `eqv`.
