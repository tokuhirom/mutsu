# Assigning a Set/Bag/Mix to an array keeps the element objects

`my @a = bag(1, 2)` filled the array with pairs keyed by the QuantHash's internal
`Int|1` strings instead of the original element objects. Array assignment now lists a
Set/Bag/Mix through `value_to_list`, as `.list` does. This also fixes
`@a (+)= (3,)` and `@a[0] (-)= set(1)` (#12399).
