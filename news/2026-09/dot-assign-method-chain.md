# `.=` continues its method chain

The statement-level `.=` parser now consumes the complete method chain, so
expressions such as `$value .= Numeric.Rat` mutate the intended value.
