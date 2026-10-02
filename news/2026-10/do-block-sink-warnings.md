# Sink warnings inside do blocks

Source `do` blocks now warn for useless values in their non-final statements, even when the block supplies a value to an initializer or a routine. When the `do` block itself is discarded, its final statement is checked too.
