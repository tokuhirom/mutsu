# A placeholder parameter's `.name` has no `^` twigil

`{ $^x + $^y }.signature.params.map(*.name)` gave `($^x, $^y)`; rakudo answers
`($x, $y)`. Code that builds source text from the parameter names (Math::NIntegrate's
`UnitCubeRescaling` makes `sub ($x, $y) { ... }` from them) then produced empty
variables and died with "Preceding context expects a term". `Parameter.name`
now drops the placeholder twigil.
