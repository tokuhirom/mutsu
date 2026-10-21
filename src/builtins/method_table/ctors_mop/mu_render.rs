//! `Mu.gist` and `Mu.raku` (ADR-11276 §9.162): the renderings a receiver
//! reaches when its own type declares none.
//!
//! The closed and open shapes answer their own `gist`/`raku` rows first;
//! these rows answer what is left, as the zero-argument rendering cascade
//! (`methods_0arg/dispatch_core_repr.rs`) did: a type object (`(Int)`, `Int`),
//! `Nil`, a parametric role, a `Signature`, a `Parameter`, a `Failure` (its
//! rows, or it throws), a `CallFrame`, a quant hash, a package stash, a
//! `Slip`, a `WhateverCode`, a `Range`, a lazy list's placeholder, a
//! `Promise`, a `Channel`, an `IO::Special` (whose `gist` is its `raku`) and
//! the other receivers with no shape, by their string form. They decline what
//! needs the interpreter: an instance (its class may declare `gist` or `raku`),
//! an enum, a code object, a `Junction`'s `raku` and a lazy list that must be
//! forced.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::value::raku_repr::{promise_raku_repr, raku_value};
use crate::value::types::is_stash_class_name;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Mu",
        name: "gist",
        arity: 0,
        handler: Handler::Narrow(gist),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    MethodRow {
        owner: "Mu",
        name: "raku",
        arity: 0,
        handler: Handler::Narrow(raku),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

/// `Mu.gist`.
// Cost: O(n), n = chars of the rendering.
fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // An `IO::Special`'s `gist` is its `raku` (`IO::Special.new("<STDOUT>")`).
    if let ValueView::Instance { class_name, .. } = target.view()
        && class_name == "IO::Special"
    {
        return crate::builtins::method_table::io_special_raku(target, &[]);
    }
    render(target, "gist")
}

/// `Mu.raku`.
// Cost: O(n), n = chars of the rendering.
fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    render(target, "raku")
}

/// The rendering `method` (`gist` or `raku`) of a receiver no closer row
/// answers.
// Cost: O(n), n = chars of the rendering.
fn render(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    if method == "gist"
        && let Some(answer) =
            crate::builtins::methods_0arg::collection_gist::collection_gist(target)
    {
        return answer;
    }
    // A collection holding a value whose `raku` is user code is its own
    // owner's late twin's (§9.194).
    if method == "raku" && crate::runtime::container_needs_raku_dispatch(target) {
        return None;
    }
    match target.view() {
        // A mixin renders through the value it wraps or the roles mixed into
        // it, which the cascade's mixin step and the interpreter resolve.
        ValueView::Mixin(..) => None,
        // An itemized lazy sequence renders the prefix it pulls inside the
        // itemization's `$( )`, which only the interpreter can.
        ValueView::Scalar(inner)
            if method == "raku" && matches!(inner.view(), ValueView::LazyList(_)) =>
        {
            None
        }
        // A parameterized role type object: raku is the bare name
        // (`Cup[EggNog]`), gist wraps it in type-object parens.
        ValueView::ParametricRole { .. } => {
            let name = raku_value(target);
            if method == "gist" {
                Some(Ok(Value::str(format!("({})", name))))
            } else {
                Some(Ok(Value::str(name)))
            }
        }
        // raku/perl and gist of the Nil value are all "Nil". (Uninitialized
        // variables hold the Any type object since PLAN 8.5, so a runtime
        // Nil here is a genuine Nil, not an uninit placeholder.)
        ValueView::Nil => Some(Ok(Value::str_from("Nil"))),
        // The `Signature` rows' renderings (`method_table::signature`).
        ValueView::Instance { class_name, .. } if class_name == "Signature" => {
            let id = crate::builtins::method_table::owner_row(
                crate::symbol::Symbol::intern("Signature"),
                crate::symbol::Symbol::intern(method),
                0,
            )?;
            crate::builtins::method_table::invoke(id, target, &[])
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Parameter" && matches!(method, "raku" | "gist") => {
            Some(Ok(Value::str(crate::value::signature::parameter_to_raku(
                &(attributes).as_map(),
            ))))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            // `gist`, `raku` and `Str` are rows (`method_table::failure`). Any other
            // method (`Numeric`, `Int`, ...) used in a value context throws the
            // wrapped exception.
            if let Some(answer) = crate::builtins::method_table::failure::answer(target, method) {
                Some(answer)
            } else if let Some(ex) = attributes.as_map().get("exception") {
                Some(Err(RuntimeError::from_exception_value(ex.clone())))
            } else {
                Some(Err(RuntimeError::new("Failed")))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "CallFrame" => {
            if method == "gist" {
                let file = attributes
                    .as_map()
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .as_map()
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!("{} at line {}", file, line))))
            } else {
                // .raku: CallFrame.new(...)
                let file = attributes
                    .as_map()
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .as_map()
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!(
                    "CallFrame.new(annotations => {{:file(\"{}\"), :line(\"{}\")}}, my => {{}})",
                    file, line
                ))))
            }
        }
        ValueView::Package(name) => {
            let resolved = name.resolve();
            let full = crate::value::user_facing_type_name(&resolved);
            if method == "gist" {
                let short =
                    crate::qualified::last_segment(crate::symbol::Symbol::intern(&full)).as_str();
                Some(Ok(Value::str(format!("({})", short))))
            } else {
                // .raku returns the full type name
                Some(Ok(Value::str(full.to_string())))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if is_stash_class_name(class_name.as_str()) && (method == "gist" || method == "Str") => {
            // Stash.gist and Stash.Str return the package name
            if let Some(ValueView::Str(name)) = attributes.as_map().get("name").map(Value::view) {
                Some(Ok(Value::str(name.to_string())))
            } else {
                Some(Ok(Value::str(String::new())))
            }
        }
        ValueView::Instance { .. } | ValueView::Enum { .. } => None,
        // The `raku` rows' renderer (`method_table::collection_render_names`).
        ValueView::Array(..) | ValueView::Seq(_) if method == "raku" => {
            crate::builtins::method_table::collection_render_names::raku(target, &[])
        }
        // Delegate to the single Slip renderer in `raku_repr` rather than
        // repeating it: this arm used to carry its own copy of the `Empty` /
        // trailing-comma rules, so the `$`-itemization the nested renderer
        // learned (`my $x = slip(5, 6); $x.raku` is `$(slip(5, 6))`) never
        // reached a top-level `.raku` call.
        ValueView::Slip(_) if method == "raku" => Some(Ok(Value::str(raku_value(target)))),
        ValueView::Junction { .. } if method == "raku" => None,
        // A WhateverCode (`*+1`, `*.abs`) renders as `WhateverCode.new` for
        // `.gist`/`.raku`/`.perl`, not the generic closure form.
        ValueView::Sub(data)
            if (method == "raku" || method == "gist")
                && matches!(
                    data.env.get("__mutsu_callable_type").map(Value::view),
                    Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
                ) =>
        {
            Some(Ok(Value::str_from("WhateverCode.new")))
        }
        // Sub/Routine/WeakSub: delegate to interpreter for proper raku/gist/Str
        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => None,
        // `gist` was answered by `collection_gist` above.
        ValueView::Pair(..) | ValueView::ValuePair(..) => {
            crate::builtins::method_table::collection_render_names::raku(target, &[])
        }
        // Delegate to raku_value which has cycle detection for self-referencing
        // hashes (e.g. %h<b> = %h).
        ValueView::Hash(..) => {
            crate::builtins::method_table::collection_render_names::raku(target, &[])
        }
        _ if target.is_range() && (method == "gist" || method == "raku") => {
            crate::builtins::method_table::collection_render::range_render(target, &[])
        }
        // A genuinely-lazy (infinite) list renders a `(...)`/`[...]`/`...`
        // placeholder rather than materializing — e.g. `[2,3].roll(*).gist`
        // (raku: `(...)`). Finite/`cat_pull` lazy lists fall through to force.
        ValueView::LazyList(ll)
            if (method == "gist" || method == "raku")
                && ll.renders_lazy_placeholder()
                // A bare lazy Seq's `.raku` reifies a prefix, which needs the
                // interpreter (`Interpreter::lazy_seq_raku`): decline.
                && !(method != "gist" && !ll.in_array_context()) =>
        {
            Some(Ok(Value::str(crate::value::lazy_list_placeholder(
                method,
                ll.in_array_context(),
            ))))
        }
        ValueView::LazyList(_) => None, // fall through to runtime to force
        // Promise has no custom gist, so `.gist` is the default `.raku` form.
        ValueView::Promise(p) => Some(Ok(Value::str(promise_raku_repr(&p.status())))),
        // Channel likewise; its bare string value reads as a type object.
        ValueView::Channel(_) => Some(Ok(Value::str_from("Channel.new"))),
        // A value with a shape is its rows' (`method_table::scalars::render`
        // and the other rendering rows): nothing here answers it.
        _ if target.dispatch_shape().is_some() => None,
        _ => Some(Ok(Value::str(target.to_string_value()))),
    }
}
