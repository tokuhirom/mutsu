# Bag.Numeric and Mix.Numeric, and numeric `==` on Bags/Mixes

`Bag`, `BagHash`, `Mix` and `MixHash` now answer `.Numeric` with their total weight, as Rakudo
does, instead of dying with "No such method 'Numeric'". Numeric `==` on a Bag or Mix now compares
those totals (`bag(1,1) == bag(2,2)` is `True`) rather than comparing structurally. Both paths share
the existing `.total` implementation.
