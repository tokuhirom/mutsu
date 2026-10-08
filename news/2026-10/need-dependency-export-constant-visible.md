# A re-exported constant stays visible after the module `need`s a dependency exporting the same name

A module that `need`ed a dependency publishing `our constant X is export` and then
exported its own `X` lost the name in its importer (`Undeclared name: X`): the
ADR-11136 visibility gate attributed `X` to the unmerged dependency and never
consulted the term key (`\X`) under which an imported sigilless constant is
recorded. `bare_name_visible_here` now honours that alias. Found through
`Mathematica::Serializer`, whose `t/03-wxf-deserializer.rakutest` now passes.
