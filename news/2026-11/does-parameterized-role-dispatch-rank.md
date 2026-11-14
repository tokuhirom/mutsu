# A role mixed in with `does R[...]` ranks as that role in multi dispatch

`$obj does R[...]` reblesses the object into a mixin type in place (#17766).
That type records the role under its application key: `R[Block]` followed by
the arguments' identity. The multi-dispatch ranking compared that key with the
bare role name in a candidate's signature. They never matched, so a wider
candidate, such as the object's own class or `Mu` with `Attribute`, won over
the role candidate. `but R[...]` was not affected.

The ranking now matches a role by the name before its brackets.

This was the last XML::Class file that differed from rakudo. XML::Class's
`is xml-serialise(&f)` trait mixes `SerialiseX[&f]` into an attribute, and
`serialise($val, SerialiseX $a)` lost to the generic element candidate, so
t/110-custom produced `<Version/>` elements instead of attributes. All of
XML::Class's test files now match raku.
