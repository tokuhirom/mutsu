# Relative qualified class names in unit packages

Qualified class and role declarations inside a unit package now register relative to that package, including their forward declaration shells. Their source spellings remain available as aliases when imported. Type references in the same package resolve the relative name before an outer type with the same spelling.
