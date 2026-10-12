# A `die` in a method's `trait_mod:<is>` handler fails the declaration

The dispatch of a method's custom `is` traits discarded every error from the
`trait_mod:<is>` call, so a handler that refuses its target (`Method::Protected`:
"Cannot apply 'is protected' to method 'b' in a role") was ignored. Only "no candidate
matches" stays silent now; any other exception propagates.
