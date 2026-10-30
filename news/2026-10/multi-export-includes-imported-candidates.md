# A module's `&name` export includes the candidates it imported

A `multi sub` declared in a module that imported the same family's `proto` now exports the imported candidates as well, as rakudo does. A `sub EXPORT` that rebuilds a module's surface from `A::EXPORT::DEFAULT::` and `B::EXPORT::DEFAULT::` therefore keeps every candidate (ake's `task` family, #15619).
