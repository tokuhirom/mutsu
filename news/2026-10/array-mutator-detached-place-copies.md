# The last by-value copies of the array mutators are gone

Two copies of `Array`'s `push`/`pop`/`shift`/... now go through the method
table's `Mut` rows on a detached place (ADR-11276 §9.147). The first is a
`pop` fallback that answered any receiver: `Foo.new.pop` returned an
"empty Array" `Failure` where Rakudo has no such method. The second
mutated a `Proxy` subclass's array attribute. Three call sites that listed
the mutator names now ask the table instead. An enum value also reaches
`Cool`'s rows through its base type, as in Rakudo, so a `Str`-valued enum
value's `sign` numifies its value.
