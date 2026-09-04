# Three associative multi-dim lvalue edges diverge from rakudo

Measured 2026-09-04 on a debug build while fixing the multi-dim chain
assignment. All three affect the *named-root* spelling too, so none of them is
specific to a subscript chain; they are separate, pre-existing gaps in how the
associative multi-dim subscript behaves as an lvalue.

## 1. `//=` ignores the multislice

Under 6.d `%h{1;2}` is a multislice, so the lvalue is a one-element `List` --
a defined value. `//=` therefore does nothing:

```raku
my %h; %h{1;2} //= 7;   # raku: {}
                        # mutsu (named root):  {"1" => ${"2" => Any}}
                        # mutsu (chain root):  %o<i>{1;2} //= 7 writes 7
```

mutsu evaluates the definedness of the *leaf* rather than the multislice
wrapper, and the two spellings do not even agree with each other.

## 2. `%h{*} = ...` should be refused

```raku
my %h; %h{*} = 5;   # raku: "Cannot assign to *, as the order of keys is
                    #        non-deterministic"
                    # mutsu: {"*" => 5}   -- stringifies Whatever into a key
```

`resolve_assign_dim` expands `Whatever` to the container's existing keys, which
is empty for a fresh hash; the fallback then stringifies the `Whatever` itself.
A `Whatever` dimension in an *assignment* to an Associative must throw.

## 3. `:delete` on a multi-dim associative subscript should throw

```raku
my %h; %h{1;2} = 5; %h{1;2}:delete;
# raku:  Cannot resolve caller postcircumfix:<{; }>(Hash:D, List:D, :delete);
#        none of these signatures matches:
#            (\SELF, @indices)
#            (\SELF, @indices, :$exists!)
# mutsu: silently no-ops, leaving {"1" => ${"2" => 5}}
```

Only `:exists` is a valid adverb on `postcircumfix:<{; }>`; mutsu accepts
`:delete` and then does nothing, which is the worst of both.

## Where to look

`src/vm/vm_var_multidim_ops.rs` (`multi_dim_assign`, `resolve_assign_dim`) for
1 and 2; the multi-dim subscript-adverb path for 3. The multislice rule itself
is already correct and version-aware (`Self::assoc_multislice`) --
`news/2026-09/associative-multidim-subscript.md` -- so 1 is about routing the
compound-assignment read through the same wrapper the plain read produces.
