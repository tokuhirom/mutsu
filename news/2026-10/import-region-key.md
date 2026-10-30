# A routine written in a block sees every kind of import the block made

`OpCode::ImportScope` now carries a region key (the unit's file plus the line of the block's
first import statement). The open import scope records it, and a routine, closure or method
written inside the block lists the keys of the blocks around it
(`CompiledCode::enclosing_import_regions`). The frame of such a routine no longer hides what
the block imported, whichever statement imported it: `need`, `import`, `use`. Before, only the
`use` of a lexically importable module was seen, through the `lexical_sets` of the scope.
The matching check on the lexical import sets (#15694) is gone. The precomp cache format
version is bumped to 57.
