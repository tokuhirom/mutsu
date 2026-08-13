use Test;

plan 1;

# `unit class Foo is repr<CStruct>;` had the same paren-only-trait-argument
# limitation as the block form (`t/is-repr-angle-bracket-trait.t`), plus its
# own separate hand-duplicated `is`/`does`/`hides` loop that discarded `repr`
# entirely regardless of syntax. A `unit class` statement extends to the end
# of the file, so this needs its own file. (The parsed `repr` value itself is
# pinned by a Rust unit test in src/parser/stmt/class/package_decl.rs, since
# `unit class ... is repr(...)` not actually setting `.REPR` at runtime is a
# separate, pre-existing gap unrelated to this angle-bracket parsing fix.)

unit class Foo is repr<CStruct>;
has uint64 $.x;

is Foo.^name, 'Foo', 'unit class: is repr<CStruct> (angle form) still declares the type';
