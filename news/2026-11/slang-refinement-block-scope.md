# A slang's action refinement ends with the block that holds its `use`

The core-action overrides a slang registers (`numish`, `INFIX-EXPR`, `PREFIX-EXPR`) used to apply to
everything after the `use` up to the end of the unit. They now stop at the end of the enclosing
block, as `overload::constant` expects ("overloading only inside its scope", #15804).
