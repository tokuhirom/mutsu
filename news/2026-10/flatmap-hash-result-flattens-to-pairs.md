# `.flatmap` flattens a Hash the mapper returns to its pairs

`.flatmap` is `.map(...).flat`, and `.flat` spills a bare Hash into its pairs. mutsu's
`dispatch_flatmap_method` flattened Arrays, Seqs, Ranges and gathers but pushed a Hash
whole, so `[$.left, $.right].flatmap({ .should-set })` (`Red::AST::AND.should-set`, under
`--> Hash()`) produced `{"guild-id\t1" => ${:user-id(1)}}` instead of
`{:guild-id(1), :user-id(1)}`, and `Model.^all.grep({ .a == 1 && .b == 1 }).create(...)`
lost its column values. An itemized Hash still stays one element.
