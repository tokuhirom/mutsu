# A package block's exit no longer walks the whole env

`OpCode::PackageScope` runs a `package`/`module`/`class` block inline. On exit
it decides which names propagate to the enclosing scope and which are package
lexicals that a sub of the block closes over. It used to decide that by
walking every entry of the env it ran in, after cloning that env on entry, so
each package block cost O(env size) (#9171).

The body now runs over a block tier of its own (`Env::open_block_tier`, the
#9170 machinery lexical blocks already use). Its exit reads back only the
names the body wrote by name. The rules for those names are unchanged:
- a write to an enclosing lexical propagates out;
- an `our` alias does not overwrite an outer lexical;
- a new `my` is recorded as a package lexical.

When no tier can be opened, the exit walks the whole env as before.

Debug build, callgrind:
- 1000 subs followed by 250 empty `package` blocks: the op went from ~8.5M to
  ~13k instructions per block.
- 1000 `our` variables followed by 250 package blocks: the whole program went
  from 13.4G to 1.9G instructions.

The op still copies and restores the frame's locals, which is O(locals).
