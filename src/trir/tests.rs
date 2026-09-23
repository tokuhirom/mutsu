//! Compiler-side pins for ADR-0110's eligibility gate.
//!
//! The Raku differential test (`t/vm/codegen/adr0110-trir-differential.t`) proves
//! the two paths *agree*; it cannot prove a routine was compiled to TRIR at
//! all, so a change that silently made every routine decline would leave it
//! green. These pin the other half: which shapes are admitted, what the chunk
//! for the ADR's own `nom-ws` looks like, and which shapes must decline.

use super::compile::TrirCompiler;
use super::{TrChunk, TrOp};
use crate::ast::Stmt;
use crate::symbol::Symbol;

/// Compile the first `sub` declaration in `src` to TRIR, or `None`.
fn chunk_of(src: &str) -> Option<TrChunk> {
    let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("test source must parse");
    first_sub_chunk(&stmts).expect("test source declares no sub")
}

/// The first `sub` declaration anywhere in `stmts`, compiled to TRIR (or
/// `None` if it declined). `Option<Option<..>>` so "no sub here" and "this sub
/// declined" stay distinguishable while the walk recurses into method bodies.
fn first_sub_chunk(stmts: &[Stmt]) -> Option<Option<TrChunk>> {
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                params,
                param_defs,
                return_type,
                body,
                ..
            } => {
                return Some(TrirCompiler::compile(
                    *name,
                    param_defs,
                    params,
                    return_type.as_deref(),
                    body,
                    super::compile::TrirScope::default(),
                ));
            }
            Stmt::ClassDecl { body, .. } => {
                if let Some(found) = first_sub_chunk(body) {
                    return Some(found);
                }
            }
            Stmt::MethodDecl { body, .. } => {
                if let Some(found) = first_sub_chunk(body) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

/// A one-word rendering of an op, so an expectation reads as the instruction
/// sequence rather than as a `Debug` dump.
fn sketch(op: &TrOp) -> String {
    match op {
        TrOp::LoadI(n) => format!("LoadI({n})"),
        TrOp::GetRefI(n) => format!("GetRefI({n})"),
        TrOp::IncRefIVoid(n) => format!("IncRefIVoid({n})"),
        TrOp::CallTr(n) => format!("CallTr({n})"),
        TrOp::CallGen(n) => format!("CallGen({n})"),
        TrOp::ConstI(v) => format!("ConstI({v})"),
        TrOp::IncIVoid(n) => format!("IncIVoid({n})"),
        TrOp::OrdAtLocal(n) => format!("OrdAtLocal({n})"),
        TrOp::AtPosIOuter(n) => format!("AtPosIOuter({n})"),
        TrOp::JumpIfFalseI(t) => format!("JumpIfFalseI({t})"),
        TrOp::Jump(t) => format!("Jump({t})"),
        TrOp::ConstObj(i) => format!("ConstObj({i})"),
        other => format!("{other:?}"),
    }
}

#[test]
fn nom_ws_compiles_to_the_typed_scanner_loop() {
    // ADR-0110 §1.4's own example. Six instructions carry the loop, and none
    // of them names a variable, a type or a callee.
    let chunk = chunk_of(
        r#"
        use nqp;
        my $ws := nqp::list_i;
        my sub nom-ws(str $text, int $pos is rw --> Nil) {
            nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
        }
        "#,
    )
    .expect("the ADR's own scanner shape must be admitted");
    let ops: Vec<String> = chunk.ops.iter().map(sketch).collect();
    assert_eq!(
        ops,
        vec![
            // `$pos` is `is rw`, so it is a REFERENCE to the caller's slot
            // (ADR-0110 §3.3's `getlexref_i`) and every read and increment of
            // it goes through that reference.
            "GetRefI(0)",
            "OrdAtLocal(0)",
            "AtPosIOuter(0)",
            "JumpIfFalseI(6)",
            "IncRefIVoid(0)",
            "Jump(0)",
            "ConstObj(0)",
            "PopObj",
            "ReturnNil",
        ]
    );
    assert_eq!(
        chunk.n_native, 1,
        "the `int` parameter is the only native slot"
    );
    assert_eq!(chunk.n_obj, 1, "the `str` parameter is the only boxed slot");
    assert_eq!(chunk.outers.len(), 1, "`$ws` is the only free variable");
    assert_eq!(chunk.outers[0].name, Symbol::intern("ws"));
    assert!(chunk.params[1].is_rw, "`$pos` is the `is rw` parameter");
}

#[test]
fn native_arithmetic_uses_typed_ops() {
    let chunk = chunk_of("my sub f(int $a, int $b) { $a * $b + ($a - $b) }")
        .expect("native int arithmetic must be admitted");
    let ops: Vec<String> = chunk.ops.iter().map(sketch).collect();
    assert_eq!(
        ops,
        vec![
            "LoadI(0)", "LoadI(1)", "MulI", "LoadI(0)", "LoadI(1)", "SubI", "AddI", "ReturnI",
        ]
    );
}

/// `nqp::substr`/`nqp::eqat` in the 3-argument shape `JSON::Fast`'s
/// `parse-string` uses (issue #8900) compile to the typed, operand-direct
/// forms rather than the generic boxed-dispatch `NqpOpGen`.
#[test]
fn substr_and_eqat_use_typed_ops() {
    let chunk = chunk_of("my sub f(str $s, int $from, int $len) { nqp::substr($s, $from, $len) }")
        .expect("nqp::substr in the 3-arg shape must be admitted");
    let ops: Vec<String> = chunk.ops.iter().map(sketch).collect();
    assert_eq!(
        ops,
        vec!["LoadObj(0)", "LoadI(0)", "LoadI(1)", "SubstrS", "ReturnObj"]
    );

    let chunk =
        chunk_of("my sub g(str $s, str $needle, int $pos) { nqp::eqat($s, $needle, $pos) }")
            .expect("nqp::eqat must be admitted");
    let ops: Vec<String> = chunk.ops.iter().map(sketch).collect();
    assert_eq!(
        ops,
        vec!["LoadObj(0)", "LoadObj(1)", "LoadI(0)", "EqAtS", "ReturnI"]
    );
}

/// Every one of these is a construct Stage 1 does not prove, so the WHOLE
/// routine declines and takes the untyped path (ADR-0110 §4). Each is listed
/// with the reason, because a future stage lifting one should have to delete
/// its line deliberately.
#[test]
fn unprovable_shapes_decline() {
    let cases: &[(&str, &str)] = &[
        (
            "a boxed nominal type needs the general binder's check",
            "my sub f(Int $a) { nqp::add_i($a, 1) }",
        ),
        (
            "a slurpy parameter",
            "my sub f(int $a, *@rest) { nqp::add_i($a, 1) }",
        ),
        (
            "a named parameter",
            "my sub f(int $a, :$flag) { nqp::add_i($a, 1) }",
        ),
        (
            "a parameter default",
            "my sub f(int $a = 1) { nqp::add_i($a, 1) }",
        ),
        (
            "a `where` constraint",
            "my sub f(int $a where * > 0) { nqp::add_i($a, 1) }",
        ),
        (
            "an `is copy` parameter",
            "my sub f(int $a is copy) { nqp::add_i($a, 1) }",
        ),
        ("an array parameter", "my sub f(@a) { nqp::elems(@a) }"),
        (
            "a nominal return type needs the general path's coercion",
            "my sub f(int $a --> Int) { nqp::add_i($a, 1) }",
        ),
        (
            "`int / int` is a Rat in Raku, not integer division",
            "my sub f(int $a, int $b) { $a / $b }",
        ),
        (
            "a boxed operand: `+` there is full multi-dispatch",
            "my sub f($a, $b) { $a + $b }",
        ),
        (
            "an attributive parameter binds to self's attribute cell",
            "class B { has $!t; method m { sub s($!t) { }; s(1) } }",
        ),
        (
            "a dynamic parameter",
            "my sub f($*level) { nqp::add_i(1, 1) }",
        ),
        (
            "the reserved invocant lexical",
            "my sub f($self) { nqp::chars($self) }",
        ),
        (
            "a statement form Stage 1 does not compile",
            "my sub f(int $a) { for ^$a { } ; $a }",
        ),
    ];
    for (why, src) in cases {
        assert!(chunk_of(src).is_none(), "must decline ({why}): {src}");
    }
}

/// A call the compiler did not resolve is not a decline: it compiles to the
/// generic form, which is what keeps a routine whose hot loop is typed from
/// being refused for the cold `die` helper it ends in (ADR-0110 Stage 2).
#[test]
fn an_unresolved_callee_compiles_to_the_generic_call() {
    let chunk = chunk_of("my sub f(int $a) { g($a) }")
        .expect("a call to an unknown routine must still be admitted");
    let ops: Vec<String> = chunk.ops.iter().map(sketch).collect();
    assert_eq!(ops, vec!["CallGen(0)", "ReturnObj"]);
    assert_eq!(chunk.calls.len(), 1);
    assert!(matches!(
        chunk.calls[0].callee,
        crate::trir::TrCallee::Generic
    ));
}

#[test]
fn mutsu_trir_off_declines_everything() {
    // The env switch is read once per process, so this asserts the gate
    // exists rather than flipping it mid-run (which `TrChunk::enabled`'s
    // `OnceLock` deliberately does not allow).
    if std::env::var("MUTSU_TRIR").as_deref() == Ok("off") {
        assert!(chunk_of("my sub f(int $a) { nqp::add_i($a, 1) }").is_none());
    } else {
        assert!(TrChunk::enabled());
    }
}
