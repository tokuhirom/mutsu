# Calling a subset of a parameterized type coerces

`subset Names of Array[Name]; Names(<a b>.Seq)` died with "No such method
'Array[Name]' for invocant of type 'Seq'": the subset call coerced by calling a
method named after the refinee, which a parameterized type does not have. It now
coerces as `Array[Name](value)` does (#15191).
