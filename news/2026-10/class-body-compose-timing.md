Eligible class body statements now run after their type has been composed,
matching Rakudo's BEGIN-time declaration behavior. This covers braced and
`unit class` declarations in the mainline and loaded units. It also adds
`ClassHOW.is_composed` and the generated local `POPULATE` method to the MOP
view after composition.
