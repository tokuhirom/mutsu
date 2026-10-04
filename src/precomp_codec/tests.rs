use super::*;
use crate::compiler::Compiler;

fn compile(src: &str) -> (crate::opcode::CompiledCode, crate::opcode::CompiledFns) {
    let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("source parses");
    Compiler::new().compile(&stmts)
}

/// Encoding, decoding and re-encoding reproduces the same bytes, for a unit
/// that exercises routines (with nested routines and closures), a class with
/// methods and attributes, and state variables.
#[test]
fn a_round_trip_reproduces_the_encoding() {
    let (code, fns) = compile(
        "sub outer($x) { my sub inner($y) { $y * 2 }; inner($x) + 1 }\n\
         class P { has $.x = 3; method double { $!x * 2 } }\n\
         my @a = (1..3).map({ $_ + 1 });\n\
         sub counter { state $n = 0; ++$n }\n\
         say outer(2), P.new.double, @a, counter();\n",
    );
    let first = encode_compiled(&code, &fns).expect("encodes");
    let (code2, fns2) = decode_compiled(&first).expect("decodes");
    let second = encode_compiled(&code2, &fns2).expect("re-encodes");
    assert_eq!(first, second);
    assert_eq!(fns.len(), fns2.len());
    assert_eq!(code.ops.len(), code2.ops.len());
}

/// Symbols cross the encoding by name: the decoded chunk names the same
/// strings, whatever ids this process interned them under.
#[test]
fn symbols_are_restored_by_name() {
    let (code, fns) = compile("sub only-here-xyzzy($a) { $a }; only-here-xyzzy(1)");
    let bytes = encode_compiled(&code, &fns).expect("encodes");
    let (_, fns2) = decode_compiled(&bytes).expect("decodes");
    let mut names: Vec<&str> = fns2.keys().map(|k| k.as_str()).collect();
    names.sort();
    assert!(
        names.iter().any(|n| n.contains("only-here-xyzzy")),
        "{names:?}"
    );
}

/// A constant that records an object id cannot be cached: in another process
/// the id may name an unrelated live object.
#[test]
fn an_identity_carrying_constant_is_refused() {
    let instance = Value::make_instance(Symbol::intern("Foo"), crate::value::AttrMap::default());
    assert!(encode(&instance).is_err());
    assert!(encode(&Value::int(42)).is_ok());
}
