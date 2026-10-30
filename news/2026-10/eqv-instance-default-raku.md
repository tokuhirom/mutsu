# `eqv` reads a differing instance attribute by its `.raku`

Rakudo's default `.raku` renders every attribute through that attribute's own `.raku`, and `Any:D eqv Any:D` compares those renderings. So two instances whose attributes read the same are `eqv` even when an attribute holds two distinct type objects that share a name (`Q.new(v => T1) eqv Q.new(v => T2)` with two generated types both named `Same`). mutsu compared attribute values structurally, where distinct type objects are never equal.

The per-slot walk stays (a hidden parent-class slot that differs still makes two instances unequal, as in Rakudo), but an attribute value the structural comparison found different is now also compared by its `.raku` rendering. A plain `List` of type objects still compares its elements (`(T1,) eqv (T2,)` stays false).

Red 0.2.7 `t/72-agg-methods` now passes: `Location.^all.map({ .temps.max(*.temperature) })` built `CASE WHEN ... THEN SELECT ... ELSE SELECT ... END` because its optimizer could not see that the two identical sub-selects (each over its own generated `TempSensor` type) were `eqv`.
