# A nested Pod block closes at its own indentation, so Pod::Load loads again

#18866 made a delimited Pod block end at the `=end` written at its `=begin`'s
indentation, as rakudo does (#18369). It nested a same-type `=begin` only at
the outer block's indentation, though. Pod::Load's SYNOPSIS has an indented
`=begin pod` / `=end pod` pair inside its `=begin pod`. The skipper saw the
inner `=end` at an unexpected indentation and raised the RakuDoc syntax error.
That left Pod::Load, Pod::Utils and about ten distributions that load them
`blocked_load`.

The skipper now keeps the indentation of every open same-type block. A nested
`=begin` opens a block at its own indentation, and an `=end` closes the
innermost one at that indentation. An `=end` anywhere else is still rakudo's
RakuDoc syntax error.
