Fix `OUR::{'&trait_mod:<is>'} := &trait_mod:<is>` re-exports so the bound multi
candidate remains visible to modules that import the package.  Custom attribute
traits re-exported this way now work when an importing class declares the trait.
