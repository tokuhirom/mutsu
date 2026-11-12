static CATALOG_A: &[BuiltinTypeInfo] = &[
    // ---- Cool itself (ADR-0051 P1): previously registered nowhere -- `.^mro`
    // for it was correct only by accident of `classhow_mro_names`' unregistered-
    // type fallback (`vec![class_name]` plus the unconditional Any/Mu append).
    // raku: `Cool.^mro` is `Cool, Any, Mu`; `Cool.^roles` is empty.
    row!("Cool", mro: ["Cool", "Any", "Mu"], roles: [], owner: ""),
    // `ObjAt` is the identity token's base class, not a Cool value.
    row!("ObjAt", mro: ["ObjAt", "Any", "Mu"], roles: [], owner: ""),
    row!("ValueObjAt", mro: ["ValueObjAt", "ObjAt", "Any", "Mu"], roles: [], owner: ""),
    // ---- Core Cool-derived scalars ----
    row!("Int", mro: ["Int", "Cool", "Any", "Mu"], roles: ["Real", "Numeric"], owner: "Int"),
    row!("UInt", mro: ["UInt", "Int", "Cool", "Any", "Mu"], roles: ["Real", "Numeric"], owner: "Int"),
    row!("Num", mro: ["Num", "Cool", "Any", "Mu"], roles: ["Real", "Numeric"], owner: "Num"),
    row!("Str", mro: ["Str", "Cool", "Any", "Mu"], roles: ["Stringy"], owner: "Str"),
    // 6.e's `Format` and `Formatter` (ADR-11276 §9.153). raku 2026.09 under
    // `use v6.e.PREVIEW` lists `Callable` in `Format.^mro` too, after `Cool`;
    // it is a role here, as everywhere in this catalog.
    row!("Format", mro: ["Format", "Str", "Cool", "Any", "Mu"], roles: ["Stringy", "Callable"], owner: ""),
    row!("Formatter", mro: ["Formatter", "Any", "Mu"], roles: [], owner: ""),
    row!("Rakudo::Internals", mro: ["Rakudo::Internals", "Any", "Mu"], roles: [], owner: ""),
    row!("Rakudo::Sorting", mro: ["Rakudo::Sorting", "Any", "Mu"], roles: [], owner: ""),
    // ---- Native types (NativeHOW): each is a subtype of its boxed type, so
    // `int32 ~~ Int` and `num ~~ Num` hold and the boxed type's methods answer.
    // raku: `int32.^mro` is `int32, Int, Cool, Any, Mu`; NativeHOW has no `.^roles`.
    row!("int", mro: ["int", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("int8", mro: ["int8", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("int16", mro: ["int16", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("int32", mro: ["int32", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("int64", mro: ["int64", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("uint", mro: ["uint", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("uint8", mro: ["uint8", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("uint16", mro: ["uint16", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("uint32", mro: ["uint32", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("uint64", mro: ["uint64", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("byte", mro: ["byte", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("atomicint", mro: ["atomicint", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Int"),
    row!("num", mro: ["num", "Num", "Cool", "Any", "Mu"], roles: [], owner: "Num"),
    row!("num32", mro: ["num32", "Num", "Cool", "Any", "Mu"], roles: [], owner: "Num"),
    row!("num64", mro: ["num64", "Num", "Cool", "Any", "Mu"], roles: [], owner: "Num"),
    row!("str", mro: ["str", "Str", "Cool", "Any", "Mu"], roles: [], owner: "Str"),
    // raku: `Bool is Int` — Numeric/Real are NOT composed directly on Bool.
    row!("Bool", mro: ["Bool", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Bool"),
    row!(
        "Rat",
        mro: ["Rat", "Cool", "Any", "Mu"],
        roles: ["Rational[Int,Int]", "Real", "Numeric"],
        owner: "Rat",
    ),
    row!(
        "FatRat",
        mro: ["FatRat", "Cool", "Any", "Mu"],
        roles: ["Rational[Int,Int]", "Real", "Numeric"],
        owner: "Rat",
    ),
    row!(
        "Complex",
        mro: ["Complex", "Cool", "Any", "Mu"],
        roles: ["Numeric"],
        owner: "Complex",
    ),
    // ---- Collections ----
    row!(
        "Array",
        mro: ["Array", "List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "Array",
    ),
    // `array` is the typed-array base (`array[int32]`, ...). It was not in this
    // catalog OR in `registry.rs`'s separate `builtin_mro_table`, so a
    // parametrized name like `array[int32]` fell all the way through both
    // tables' fallbacks to `[name]` or `[name, Any, Mu]` -- never reaching this
    // row's own ancestry (ADR-0019 E2b twelfth slice: the fallback that strips
    // a `Base[T]` name and splices `Base`'s catalog chain is what actually
    // reaches it, in `receiver_class.rs`/`registry.rs`). raku: `array.^mro` is
    // `array, Cool, Any, Mu`. (`CArray` is NativeCall's, declared by the
    // upstream module, not a core type.)
    row!(
        "array",
        mro: ["array", "Cool", "Any", "Mu"],
        roles: ["Positional"],
        owner: "",
    ),
    row!(
        "List",
        mro: ["List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "List",
    ),
    // `Hash is Map` — the legacy `builtin_type_mro_chain` table omits `Map` from
    // Hash's chain (V1 divergence #1); the catalog follows raku, which includes it.
    row!(
        "Hash",
        mro: ["Hash", "Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    row!(
        "Map",
        mro: ["Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    // A pseudo-package view of a lexical pad (`MY::`, `CALLER::`, `UNIT::`,
    // ...). Raku keeps it apart from the package symbol table `Stash`: they
    // are SIBLINGS under `Map`, not parent and child (measured against
    // rakudo, `PseudoStash.^mro` is `(PseudoStash Map Cool Any Mu)`), so
    // `PseudoStash` must never be modelled as a `Stash` subclass.
    row!(
        "PseudoStash",
        mro: ["PseudoStash", "Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    // raku (2026-09-29): `Stash.^mro` is `(Stash Hash Map Cool Any Mu)`,
    // `Stash.^roles` is `(Associative Iterable)`.
    row!(
        "Stash",
        mro: ["Stash", "Hash", "Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    row!(
        "Range",
        mro: ["Range", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "Range",
    ),
    row!(
        "Seq",
        mro: ["Seq", "Cool", "Any", "Mu"],
        roles: ["Sequence", "PositionalBindFailover", "Iterable"],
        owner: "",
    ),
    // raku: `Pair` does NOT inherit Cool (unlike the legacy `builtin_type_mro_chain`
    // and `builtin_type_distance` tables, both of which insert Cool — V1 divergence).
    row!("Pair", mro: ["Pair", "Any", "Mu"], roles: ["Associative"], owner: ""),
    row!(
        "Set",
        mro: ["Set", "Any", "Mu"],
        roles: ["Setty", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "SetHash",
        mro: ["SetHash", "Any", "Mu"],
        roles: ["Setty", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Bag",
        mro: ["Bag", "Any", "Mu"],
        roles: ["Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "BagHash",
        mro: ["BagHash", "Any", "Mu"],
        roles: ["Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Mix",
        mro: ["Mix", "Any", "Mu"],
        roles: ["Mixy", "Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "MixHash",
        mro: ["MixHash", "Any", "Mu"],
        roles: ["Mixy", "Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Slip",
        mro: ["Slip", "List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "List",
    ),
    row!(
        "HyperSeq",
        mro: ["HyperSeq", "Any", "Mu"],
        roles: [
            "ParallelSequence[HyperToIterator]",
            "Iterable",
            "Sequence",
            "PositionalBindFailover",
        ],
        owner: "",
    ),
    row!(
        "RaceSeq",
        mro: ["RaceSeq", "Any", "Mu"],
        roles: [
            "ParallelSequence[RaceToIterator]",
            "Iterable",
            "Sequence",
            "PositionalBindFailover",
        ],
        owner: "",
    ),
    // ---- Code/Callable family ----
    // raku: `Sub`'s chain is Sub -> Routine -> Block -> Code -> Any -> Mu, with
    // `Callable` a composed ROLE (not an MRO link) — the legacy
    // `builtin_type_distance` table interleaves `Callable` into the chain itself
    // (V1 divergence).
    row!(
        "Sub",
        mro: ["Sub", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Method",
        mro: ["Method", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Submethod",
        mro: ["Submethod", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Routine",
        mro: ["Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Block",
        mro: ["Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "WhateverCode",
        mro: ["WhateverCode", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!("Code", mro: ["Code", "Any", "Mu"], roles: ["Callable"], owner: "Code"),
    // raku: `Regex` chain is Regex -> Method -> Routine -> Block -> Code -> Any -> Mu.
    // Both legacy tables (`builtin_type_mro_chain`, `builtin_type_distance`) already
    // spell this correctly (no divergence here).
    row!(
        "Regex",
        mro: ["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    // raku (2026-09-29): `Macro.^mro` is `(Macro Routine Block Code Any Mu)`.
    row!(
        "Macro",
        mro: ["Macro", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    // ---- Junction: raku skips `Any` entirely (Junction -> Mu directly) ----
    row!("Junction", mro: ["Junction", "Mu"], roles: [], owner: ""),
    // ---- Nil: distinct from the `Any` that `value_type_name` folds it to today ----
    row!("Nil", mro: ["Nil", "Cool", "Any", "Mu"], roles: [], owner: ""),
    // `Failure` is never declared as a real class anywhere in mutsu (prelude or
    // Rust) -- it is built purely via `Value::make_instance(Symbol::intern("Failure"), ...)`
    // wherever a native method needs one, so the class registry has no model of
    // its ancestry and `class_mro("Failure")` answers just `["Failure"]` with no
    // continuation to `Any`/`Mu` at all (found via ADR-0019 E2b: the
    // `native_call_unmodeled` counter never reached zero for `Failure`'s
    // `Any`-declared universal methods -- `so`/`defined`/`sink`/... -- no matter
    // how many rows were added, because the chain walk never got past
    // `Failure` itself). raku: `Failure ISA Nil` (`Failure.new.^mro` is
    // `Failure, Nil, Cool, Any, Mu`), which this catalog row supplies via
    // `class_chain`'s direct `builtin_type_info` lookup, bypassing the
    // registry entirely for this type the same way `Nil` above already does.
    row!("Failure", mro: ["Failure", "Nil", "Cool", "Any", "Mu"], roles: [], owner: ""),
    // `Exception` is a name every built-in `X::*` exception type registers as
    // its parent (`BUILTIN_PARENT_TYPES` in `registration_class_decl.rs`),
    // but "Exception" itself is never registered as an actual class in the
    // registry -- so `compute_class_mro`'s implicit-`Any` rule (which only
    // fires for a class actually present in `self.classes`) never applies to
    // it, and every `X::*` type's registry MRO dead-ends at `Exception` with
    // no `Any`/`Mu` continuation (e.g. `X::AdHoc`'s registry MRO was
    // `["X::AdHoc", "Exception"]`). This catalog row lets
    // `class_chain_with_catalog_tail`'s splice logic supply the missing tail
    // for every such type in one place, the same mechanism the `Failure` row
    // above uses. raku: `Exception.^mro` is `Exception, Any, Mu`.
    row!("Exception", mro: ["Exception", "Any", "Mu"], roles: [], owner: ""),
    // `CX::Warn` (and the sibling `CX::*` control-exception types) is built
    // purely via `Value::make_instance` with no registered parent at all
    // (unlike `X::AdHoc`, which at least registers `is Exception` even
    // though `Exception` itself was unregistered) -- its registry MRO was
    // the bare `["CX::Warn"]`, so the `Exception` splice above never
    // triggers for it (its registry chain never mentions `Exception`).
    // raku: `CX::Warn.^mro` is `CX::Warn, Exception, Any, Mu`.
    row!(
        "CX::Warn",
        mro: ["CX::Warn", "Exception", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
];
