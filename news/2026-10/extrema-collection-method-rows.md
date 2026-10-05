The built-in method table now handles plain collection extrema (`min`, `max`,
`minpairs` and `maxpairs`) and eager List values through shared rows, while
unsupported and user-dispatched receivers retain their interpreter fallback.
