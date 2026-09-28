# Validate named arguments to placeholder routines

Routines and blocks with implicit placeholder signatures now reject unexpected named arguments. A body that reads `%_` still receives the implicit named slurpy, and a named placeholder consumes its matching argument. This corrects calls that previously discarded a surplus named argument or silently captured it without a named slurpy.
