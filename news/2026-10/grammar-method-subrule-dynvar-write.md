# A grammar method called as a subrule can update the caller's `$*` variables

A grammar method reached as a subrule — a `method foo` behind `<foo>`, or a
user `method ws` reached through sigspace — ran over an isolated copy of the
env, so an assignment to a dynamic variable of the caller was thrown away
with the copy. The "good parse errors" idiom
(`method ws { $*HIGHWATER = self.pos if self.pos > $*HIGHWATER; callsame }`,
DSL::Shared::Roles::ErrorHandling) therefore always reported position 0.

Such calls (and a custom-HOW `find_method` with the wrapper it returns) now
run over a block tier of the env, and on return the writes to dynamic
variables the caller already binds are replayed onto the caller's env. The
replay reads the tier's own overlay, so its cost follows what the method
wrote, not the size of the visible scope. A `my $*X` declared inside the
method and every lexical stay isolated as before (#11326).
