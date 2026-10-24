# Pakku::Core loads

`use Pakku::Core` failed with "Unable to parse expression in array composer".
Two causes: a module's own `use NativeCall` leaked NativeCall's exported `bool`
term to every importer of that module (so `bool 'X', $_` in Pakku::Core parsed as
two terms next to a user `my sub bool`); imports are lexical, so only a
`sub EXPORT` hook can now hand an imported value term on. And
`Distribution::Locally` was rejected as a role-method parameter type.
Pakku's cache, cmd, log and network test files now pass (0 before).
