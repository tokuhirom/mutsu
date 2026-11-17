//! `Code`'s `of`, `returns`, `arity`, `count` and `signature`, `line`, `file` and `name` (ADR-12523, slices 1-4), the
//! pure `Capture`, `clone` and `static_id` (slice 5), and `Str` (slice 6).
//!
//! A code object is a `Sub` (a routine, closure or block, with or without a
//! body of its own), a `&name` handle on a routine reached through the
//! registry, or a regex. All of them are `Code` and answer the declared return
//! type, `Mu` without one. They have no dispatch shape, so the rows are
//! reached through their owner: [`answer`] is the entry for the callable
//! dispatch and for the regex fallback.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Code",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("of", of),
    row!("returns", of),
    row!("arity", arity),
    row!("count", count),
    row!("signature", signature),
    row!("line", line),
    row!("file", file),
    row!("name", name),
    row!("Str", str_),
    MethodRow {
        owner: "Code",
        name: "POSITIONS",
        arity: 1,
        handler: Handler::Interp(positions),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Code",
        name: "set_name",
        arity: 1,
        handler: Handler::Interp(set_name),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // The pure rows: they read nothing but the value, so the zero-argument
    // cascade reaches them through [`pure_answer`].
    MethodRow {
        owner: "Code",
        name: "Capture",
        arity: 0,
        handler: Handler::Narrow(capture),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Code",
        name: "clone",
        arity: 0,
        handler: Handler::Narrow(clone),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Code",
        name: "static_id",
        arity: 0,
        handler: Handler::Narrow(static_id),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];

/// `Block` declares `of`, `returns` and `clone` itself: `Code`'s answers, found
/// by owner only (a callable's owner list is `Code`, so these are the table's
/// statement of Rakudo's declaration, not a second path).
pub(super) static BLOCK_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Block",
        name: "of",
        arity: 0,
        handler: Handler::Interp(of),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Block",
        name: "returns",
        arity: 0,
        handler: Handler::Interp(of),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Block",
        name: "clone",
        arity: 0,
        handler: Handler::Narrow(clone),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];

/// The answer of the row `target`'s class gives `method`, found through the
/// class's owner chain (`code_class::owners_of`), or `None` when `target` is
/// not a code object or its class has no such row.
// Cost: O(1) to find the row, plus the handler's own cost.
pub(crate) fn answer(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let owners = super::code_class::owners_of(target)?;
    crate::builtins::method_table::invoke_owner(interp, owners, method, args, || target.clone())
}

/// Answer the nearest pure row of the receiver's code class, or decline when
/// the receiver is not code or its nearest method needs the interpreter.
// Cost: O(o), o = owners (at most five), plus the handler's own cost.
pub(crate) fn pure_answer(target: &Value, method: Symbol) -> Option<Result<Value, RuntimeError>> {
    let owners = super::code_class::owners_of(target)?;
    crate::builtins::method_table::invoke_owner_pure(owners, target, method)
}

/// `Code.of` and `Code.returns`: the return type the routine declares, the type
/// object of its spelling in the scope it was declared in, or `Mu`.
// Cost: O(1) for a code object with no declared return type; otherwise one
// lookup of the type's spelling in the closure's scope, O(e), e = bindings it
// searches.
fn of(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mu = || Some(Ok(Value::package(Symbol::intern("Mu"))));
    let data = match target.view() {
        ValueView::Sub(data) => data.clone(),
        ValueView::WeakSub(weak) => weak.upgrade()?,
        // A name-based handle and a regex carry no return type of their own.
        _ => return mu(),
    };
    if let Some(signature) = data.routine_cell.bound_signature() {
        return super::signature::returns(&signature, &[]);
    }
    if let Some(ty) = data.routine_cell.return_type() {
        return Some(Ok(ty));
    }
    let type_name = interp
        .callable_return_type(target)
        .unwrap_or_else(|| "Mu".to_string());
    // `--> C[T]` for a `C` with its own `^parameterize` denotes the type object
    // that meta-method returns, not a name.
    if let Some(ty) = interp.meta_parameterized_type(&type_name) {
        return Some(Ok(ty));
    }
    // The return constraint is recorded by its source spelling; a lexical type
    // (`my subset ofTest ...; --> ofTest`) lives under a mangled storage name
    // (ADR-0047), so answer the type object the spelling is bound to in the
    // closure's scope, which is the one the bare `ofTest` term evaluates to.
    // Otherwise it is the type object the bare spelling evaluates to here.
    let type_name = match data.env.get(&type_name).map(Value::view) {
        Some(ValueView::Package(p)) if p.as_str().contains('\u{0}') => p.resolve().to_string(),
        _ => {
            if let Some(term) = interp.imported_type_term(&type_name) {
                return Some(Ok(term));
            }
            interp.lexical_env_remap_name(&type_name)
        }
    };
    Some(Ok(Value::package(Symbol::intern(&type_name))))
}

/// `Code.arity`: the number of required positional parameters (the smallest over
/// a multi dispatcher's candidates).
// Cost: O(p) for a routine with p parameters; O(c * p) for a multi dispatcher
// with c candidates.
fn arity(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_arity_count(target, "arity")
}

/// `Code.count`: the number of positional parameters (the largest over a multi
/// dispatcher's candidates), `Inf` with a slurpy.
// Cost: O(p) for a routine with p parameters; O(c * p) for a multi dispatcher
// with c candidates.
fn count(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_arity_count(target, "count")
}

/// `Code.signature`: the routine's `Signature` (a multi dispatcher answers its
/// proto's, a multi method's several candidates a junction of theirs).
// Cost: O(p) for a routine with p parameters; O(c * p) for a multi method's
// dispatcher with c candidates.
fn signature(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_signature(target)
}

/// `Code.line`: the line the routine was declared on, `Nil` when none is recorded.
// Cost: O(1) for a routine carrying its own location; otherwise one registry
// lookup by name.
fn line(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_line_file(target, "line")
}

/// `Code.file`: the file the routine was declared in, `Nil` when none is recorded.
// Cost: O(1) for a routine carrying its own location; otherwise one registry
// lookup by name.
fn file(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_line_file(target, "file")
}

/// `Code.name`: the routine's name, `""` when anonymous.
// Cost: O(1) for a routine with its own name; O(n) to unqualify and format a
// handle's name, n = its length.
fn name(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_name_value(target).map(Ok)
}

/// `Code.Str`: the routine's name, with Rakudo's coercion warning.
// Cost: O(n), n = chars of the name.
fn str_(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_str(target)
}

/// `Code.set_name($name)`: renames the code object itself, so every alias of
/// it (and a method `^add_method` installed from it) reports the new name
/// (#9479, PDF::COS::Tie's `&accessor.set_name($key)`). Answers the name.
// Cost: O(n), n = chars of the new name (interned), plus
// `Interpreter::rename_code`'s for a routine.
/// `Code.POSITIONS(\list)`: the index a `WhateverCode` subscript stands for,
/// the code called with the element count of `list` (`$carray[*-1]` indexes
/// `$block.POSITIONS(array)`), #16488.
// Cost: O(1) plus the arguments' `.elems` and the code's own cost.
fn positions(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    // `list` can be any `Positional`, including a user-level container (a
    // `CArray` is a Raku class), so its length is asked by method name, as
    // `elems($list)` does.
    let elems = match interp.call_method_with_values(args[0].clone(), "elems", vec![]) {
        Ok(n) => n,
        Err(e) => return Some(Err(e)),
    };
    Some(interp.call_sub_value(target.clone(), vec![elems]))
}

fn set_name(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let name = args[0].to_string_value();
    match target.view() {
        ValueView::Sub(_) => {
            interp.rename_code(target, &name);
            Some(Ok(Value::str(name)))
        }
        ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => {
            if target.set_regex_name(Symbol::intern(&name)) {
                Some(Ok(Value::str(name)))
            } else {
                // Unreachable for a value built by `Value::regex`, a literal
                // or an adverbed regex: every one of them carries a payload
                // with a name cell.
                Some(Err(RuntimeError::new(
                    "Cannot set_name on a regex value without a code-object payload",
                )))
            }
        }
        // A method object renames its method table entry (§9.167).
        ValueView::Instance { .. } => {
            interp.method_object_code_row(target, "set_name", args.to_vec())
        }
        _ => None,
    }
}

/// `Code.Capture`: a code object cannot be unpacked.
// Cost: O(1).
fn capture(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let type_name = crate::value::types::what_type_name(target);
    Some(Err(
        crate::builtins::methods_0arg::coercion::cannot_capture(&type_name),
    ))
}

/// `Code.clone`: a `Sub` is copied as a new routine object, with an id of its
/// own so its state variables are independent and its own composition cell,
/// starting from the original's (ADR-11827 section 2.3). A regex and a name-based
/// `&name` handle carry no state of their own and are returned as they are.
// Cost: O(1) for a `Sub` (the captured environment is shared, not copied).
fn clone(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Sub(data) => {
            let mut new_data = (**data).clone();
            new_data.id = crate::value::next_instance_id();
            new_data.routine_cell = new_data.routine_cell.forked();
            // The copy owns a new state scope, so its disposal is its own to
            // report (the guard it cloned would reap the ORIGINAL's scope).
            if new_data._state_scope_guard.is_some() {
                new_data._state_scope_guard = Some(
                    crate::value::state_scope_reaper::StateScopeGuard::new(new_data.id),
                );
            }
            Some(Ok(Value::sub_value(crate::gc::Gc::new(new_data))))
        }
        _ => Some(Ok(target.clone())),
    }
}

/// `Code.static_id`: the identity of the code the object runs, shared by every
/// closure clone of one block and every handle on one routine (rakudo answers
/// its static code object's id). It is a hash of what the code is -- its
/// signature and body, and where and under which name it is declared -- not
/// of a compiled object's address: a unit whose CHECK phase rewrote `$*CU`
/// runs again from the rewritten tree, and the routines it compiles anew must
/// keep the identity the CHECK phase saw (Red's compiled-callable guard).
// Cost: O(p) for a routine, p = parameters; O(b) for a closure, b = size of
// its body AST.
fn static_id(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Sub(data) = target.view() else {
        return crate::builtins::method_table::identity_where(target, &[]);
    };
    // A routine is one declaration of its name: the place it is declared and
    // its signature tell it apart (from the other candidates of a multi). Its
    // body is not hashed -- the CHECK phase's copy and the restarted unit's
    // carry it in different shapes (an AST, compiled bytecode).
    //
    // A closure is told apart by its signature and body and its line, not its
    // file or package: the closure a block node's `compile-time-value` makes in the CHECK
    // phase is compiled where the phaser's code runs (Red's module).
    // A named routine (not a block or an anonymous sub) is one declaration too,
    // compiled or not: the CHECK phase's code object of a block-nested `sub`
    // (`compile-time-value`) is built from its node, not from bytecode.
    let routine = data.compiled_routine.is_some()
        || data.body.is_empty()
        || (!data.name.is_empty() && !data.is_bare_block);
    let content = if routine {
        crate::ast::stable_hash::stable_hash(&*data.params)
    } else {
        crate::ast::function_body_fingerprint(&data.params, &data.param_defs, &data.body)
    };
    let id = crate::ast::stable_hash::stable_hash(&(
        content,
        routine.then(|| (data.package.resolve(), data.name.resolve())),
        routine.then_some(data.source_file.as_deref()),
        data.source_line,
    ));
    Some(Ok(Value::int(id as i64)))
}
