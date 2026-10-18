# A module loaded inside a block keeps its `proto sub`s

When a bare block exits, mutsu restores the routine registry to the snapshot
taken when the block began. It then puts back the routines of any module the
block loaded (`module_registered_functions`), because the module stays in
`loaded_modules` and a later `use`/`require` of it is a no-op. The proto
tables were restored wholesale, with nothing put back. So a module first
loaded by a `require` inside a block lost its `our proto sub`s when the block
exited, while its candidates stayed, and `::('M::&f')` read Nil in every later
scope (#13877).

Module loads now record the package-qualified protos they register
(`module_registered_protos`), and the block-exit restore reinstates them.
BDD::Behave lazy-loads its DSL from inside the first `describe` call, so this
had broken every later `it`. Its `t/hooks/hooks.rakutest`,
`t/shared-contexts/shared-contexts.rakutest`, `t/let/subject.rakutest`,
`t/focus-skip/focus.rakutest` and `t/mocks/allow.rakutest` now pass.
