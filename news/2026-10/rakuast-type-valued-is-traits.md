RakuAST now round-trips parameterized `is TYPE` traits on aggregate variables
and attributes, and qualified declared types on variables. Lowering restores
the existing declaration type spelling, preserving container and attribute
behavior. The frontend ratchet increased to 7,939 of 7,984 files.
