# A grammar rule's code reads the module it was declared in

A grammar declared in a module can run `{ … }` blocks and `<?{ … }>`
assertions that read the module's file-scope `my` variables. Those
variables used to read as empty once the parse ran after the module had
finished loading — for example, a parse started from a sub the module
exports, or from the program (#13915). The code was resolved in whatever scope
ran the parse, not in the scope the rule was declared in. Rainbow's tokenizer
ran into this: it reads a module-level `%quote-pairs` from a token.

```raku
# GM.rakumod
my $s = 5;
grammar GM { token TOP { x { say "s=", $s } } }
sub t is export { GM.parse('x') }

# program
use GM; t();   # s=5 (was s=(Any))
```

The routine frame of a parse's start rule now records the file the rule was
declared in. A code atom running in a subrule now switches the running
frame's defining file to that of its own rule. Its free variables then
resolve through that file's compunit bucket, as a method or sub of the module
already did. An inherited rule reads its own module's lexicals, while the
derived grammar's rules read their own scope.

Rainbow's `t/sub-langs.rakutest` `rakudoc-lang` case now matches rakudo. Its
`bugs` and `whatever-star` tokenize cases do too. The remaining differences are
#13928.
