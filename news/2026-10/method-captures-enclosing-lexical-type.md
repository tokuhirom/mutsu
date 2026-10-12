# A method names its enclosing scope's lexical types through its declaration

A method of a class or role declared in a block could name another `my class` of that
block only while the block was on the call stack. Called later, `Two.new` in the method
died with `Undeclared name` (or `Could not find symbol '&One' in 'GLOBAL::Mock'` for a
qualified name), because the bareword was resolved through the caller's env chain.

The compiler now tracks the lexical types (`my class`/`my role`/`my grammar`) in scope
(`lexical_types_in_scope`, with the same per-block lifecycle as `lexical_type_shadows`).
A method body reads such a type by name, and the declaration-time method capture
(`declared_method_capture_envs`) keeps the type object it denotes at the declaration.
That is the mechanism #11961 used for a lexical class's own name; it now covers every
enclosing lexical type, and a role declared with no slot lexicals in scope captures too.

This unblocks #13001 (zef's `t/repository.rakutest`, whose mock repositories are
`my class`es named from a `but role :: { method plugins { ... } }` mixin) (#13014).
