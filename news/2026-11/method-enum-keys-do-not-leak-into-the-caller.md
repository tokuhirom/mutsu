# The keys of an enum declared in a method no longer leak into the caller

```raku
class A { method n { enum Mode2 <Normal Other>; 1 } }
enum Volume (:Silent(-2) :Normal(0) :Verbose(1));
A.n;
say Normal.^name;   # mutsu: Mode2   raku: Volume
```

`merge_method_env` copies every key a method wrote into its frame back into
the caller's env except the method's own locals, so the `__mutsu_enum_bare_*`
key of a method-local enum (`enum` and `my enum` alike) shadowed the caller's
own enum key after the call. A method's enum is installed in the method's
package (`record_package_enum_key` already records it there) and is the method
body's lexical, so both merge predicates (the fast path and the primary path)
now treat the bare enum keys as frame-local. A plain sub's enum is still a
`GLOBAL` symbol, as in rakudo.

Found while fixing TAP (#16361). Pinned by
`t/oo/method/method-enum-keys-do-not-leak-into-caller.t`.
