# A statement modifier after `has A: has B ...` belongs to the whole statement

`has Int $.a: has Str $.b = "q" if 1;` panicked in `.AST`
(`attribute view requires HasDecl`): the parser attached the modifier to the
nested declaration, the arglist of the first, so the colon-argument view met a
`Modified` member it could not convert. In rakudo the modifier governs the whole
statement and shows on the first declaration. The parser now moves the
modifiers of a nested `has` onto the outer declaration, and
`rakuast::attribute_group::view_colon` returns the visible declaration's
statement, so its modifiers are kept (the gist matches rakudo). Issue #15600.
