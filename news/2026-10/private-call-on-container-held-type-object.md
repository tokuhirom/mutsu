# A private call whose receiver sits in a container cell dispatches on the value it holds

A method reached through a `handles` delegate that returns a type object (CSS::Grammar::Actions'
`method build handles<token node list> { CSS::Grammar::AST }`) can run with `self` held in a
container cell when the call comes through an exported method called as a sub. Its
`self!terms(...)` then died with "Calling private method 'terms' must be fully qualified".
The private-call dispatch now looks through the cell to the type object or instance.
CSS::Selector::To::XPath's exported `selector-to-xpath` gets past this; it then stops on a
separate problem (a grammar object arriving as `$/`), filed separately.
