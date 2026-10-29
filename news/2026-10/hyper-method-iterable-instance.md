# `>>.method` descends into a nested user `Iterable`

`$adam.kids>>.kids>>.value` (Red's `t/23-metamodel-model.rakutest`) died with "No such method 'value'
for invocant of type 'Tree::ResultSeq'": a hyper method call walked a top-level `Iterable` instance
through its `iterator`, but an instance that was itself an element of the list was treated as a leaf.
Elements that compose `Iterable` are now walked through their `iterator` at every nesting level.
