# A BOOT scalar box stored as an array or hash element is its high-level value

`%h<k> = $box`, `@a[0] = $box`, `@a.push($box)` and `%h = k => $box` kept the raw
`BOOTInt`/`BOOTNum`/`BOOTStr` box where rakudo stores the `Int`/`Num`/`Str`. The element-store
itemization (`Value::itemize_for_element_store`), the indexed-assignment op and `push` now hllize
it. Scalar assignment keeps the raw box, as in rakudo (#12881).
