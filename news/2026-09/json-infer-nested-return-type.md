# JSON::Infer's nested `Attribute` return types resolve correctly

`JSON::Infer` 0.1.2's `JSON::Infer::Attribute.new-from-value` and
`JSON::Infer::Class.new-attribute` methods declared `--> Attribute`.  Rakudo
resolves that short name in the enclosing class, where it means
`JSON::Infer::Attribute`; mutsu compared the return value with the unrelated
core `Attribute` type and aborted both test files.

Method return constraints now use the same declaring-class type resolution as
parameter constraints.  `JSON::Infer` moves from 2/4 to 4/4 baseline files
under mutsu (129/129 assertions); its network-dependent `t/050-infer.t`
remains `no_baseline` because Rakudo fails it independently.

Pinned by `t/oo/class/nested-class-return-type.t`, which uses the colliding
core name `Attribute` and passes under both Rakudo and mutsu.
