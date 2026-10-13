Single-quoted strings now scan embedded `\q` quotes across the containing
source. This matches Rakudo when a `\q"` delimiter is closed later in the file,
including the malformed-source recovery exercised by BDD::Behave's EventStream
tests.
