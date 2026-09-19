# Capitalized QuantHash coercers flatten scalar Lists

`Set(...)` now flattens a scalar `List` in its slurpy list context, including
Lists whose elements are themselves one-item Lists. This keeps Apriori-style
singleton sets usable when upstream modules build them from mapped paths, while
method coercion still keeps an explicitly itemized nested `List` as one object
key.
