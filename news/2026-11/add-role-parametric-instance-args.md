# `^add_role` keeps the arguments of a parametric role instance

`Metamodel::ClassHOW.^add_role(Numish[* * 10])` reduced the role value to its type
name (`Numish[WhateverCode]`) and re-resolved the role from that name, so the
argument *values* were lost and the group's stub role was picked ("No matching
candidate found for the parametric role"). The evaluated arguments of the
`ParametricRole` value now travel to the candidate resolution
(`add_role_to_class_with_args`), as they do for `class K does Numish[* * 3]`. (#17007)
