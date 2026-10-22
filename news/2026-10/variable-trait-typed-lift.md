# Variable traits: typed lift steps, once-only handlers in methods and roles

The ADR-12131 variable-trait lift no longer passes information through synthetic trait
names (`__site`, `__replay_phasers:<KIND>`, `__seeded_decl`, `__seed_*`). The declaration
site, the phaser replay and the seed stash/restore are typed `Stmt::VarTrait` nodes
(`VarTraitOp`) that compile to one `VarTraitCtl` opcode, so no consumer recognises strings
by prefix any more (#14463).

The lift now also reaches the bodies of methods, submethods and the routines of a role: a
`trait_mod:<is>(Variable ...)` handler runs once, before the body can be entered, and its
`ENTER` phasers replay on every call instead of the handler running on every call. The
`Variable.block` handle reports the owning code object's type (`Sub`, `Method`,
`Submethod`, `Block`), as Rakudo does (#14462). A trait directly in a role body and
user traits named `__...` are tracked in #14532.
