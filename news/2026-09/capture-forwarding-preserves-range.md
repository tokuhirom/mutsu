# Capture forwarding preserves Range arguments

Fixed capture forwarding with `|c` so a `Range` stored in a positional
`Capture` remains one argument instead of being expanded into its elements.
