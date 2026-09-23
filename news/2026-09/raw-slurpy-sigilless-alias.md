# Sigilless aliases can bind raw slurpy elements

Raw slurpy parameters now preserve writable element cells for sigilless aliases
selected through a conditional subscript, and their mutations are written back
to the caller even when the call environment itself was changed in place.
