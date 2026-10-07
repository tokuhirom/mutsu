# A one-element array literal keeps a Capture whole

`[\(:a(1))]` produced an empty array because the single-element array literal flattened
the Capture into its positionals. A Capture is not Iterable, so it is now stored as one
element. Found through SQL::Builder, whose `where([\(:or[...])])` sub-group syntax lost
its clause; all 11 of its test files now pass.
