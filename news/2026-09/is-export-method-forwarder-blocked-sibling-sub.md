# An is-export method's forwarder sub falsely blocked a same-named class sub

`method NAME(...) is export` registers an importable sub-form for the method
(`register_exported_operator_method_sub`), so `import ClassName` can copy it
into the importing package. That forwarder is stored under an arity-suffixed
registry key (`Class::NAME/arity`), mirroring the key shape genuine `multi
sub`/`multi method` candidates use, purely so the export machinery's prefix
scan can find it later.

That shape was coincidental, not semantic, but `register_sub_decl_with_metadata`'s
collision check could not tell the difference: an ordinary, unrelated `sub
NAME(...)` declared elsewhere in the same class body (a different namespace
from the method in real Raku — confirmed against rakudo, where the two
coexist without complaint) saw the forwarder's key and concluded a `multi`
family with this name already existed, rejecting the plain `sub` with
"Redeclaration of routine ... Did you mean to declare a multi-sub?".

Fixed by tracking the forwarder's own registry keys
(`Interpreter::method_export_forwarder_keys`) and excluding them from that
collision scan.

Found while reproducing `ML::AssociationRuleLearning`'s vendored
`ML::TriesWithFrequencies` module for
[#8777](https://github.com/tokuhirom/mutsu/issues/8777): its `Trie` class
declares exactly this shape for `leafQ` (`method leafQ(...) is export` beside
a plain `sub leafQ(ML::TriesWithFrequencies::Trie $tr --> Bool)`), which made
the whole module fail to load — blocking any measurement of the Apriori
performance question #8777 was filed to investigate.

Pinned by `t/modules/import-export/exported-method-sibling-sub.t`.
