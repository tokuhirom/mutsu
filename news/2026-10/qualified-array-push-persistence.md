# Persist pushes through qualified array names

Pushing to an undeclared package-qualified array inside a routine now survives the routine's frame. The package store keeps the vivified itemized array, while a qualified spelling of an explicitly declared `our @a` continues to share its ordinary array container.
