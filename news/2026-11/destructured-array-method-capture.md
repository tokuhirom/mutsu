# Destructured array targets keep their method captures

An array target in a destructuring declaration starts as an empty `ArrayLiteral` in the parser's expanded form. The BEGIN prologue's declaration split did not recognize that as an uninitialized array, so it left the whole destructuring declaration after a role-composing class had registered its methods. A method that pushed to the captured array then changed a different container from the one the mainline read.

The static declaration check now accepts the empty array form. The destructuring initializer still runs at its source position, while the class method and mainline share the target's container.
