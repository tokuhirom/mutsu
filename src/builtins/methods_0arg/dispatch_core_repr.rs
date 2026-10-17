/// Representation methods: `gist` and `raku` of the receivers no row answers.
use crate::value::{RuntimeError, Value, ValueView};

use super::raku_repr::{promise_raku_repr, raku_value};
use crate::value::types::is_stash_class_name;

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if !matches!(method, "gist" | "raku") {
        return None;
    }
    if method == "gist"
        && let Some(answer) = super::collection_gist::collection_gist(target)
    {
        return Some(answer);
    }
    Some(match target.view() {
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
            crate::builtins::method_table::signature::answer(target, method)
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
        // The renderers the quant hashes' rows share. The rows decline an
        // element that may carry a user `gist`/`raku`; this arm renders it
        // with the default form, as it always did.
        ValueView::Bag(..) | ValueView::Set(..) | ValueView::Mix(..) => {
            if method == "gist" && crate::Interpreter::quanthash_needs_element_dispatch(target) {
                // The interpreter resolves each element's own `gist` first.
                None
            } else if method == "gist" {
                Some(Ok(Value::str(
                    crate::value::gist::setbagmix_gist(target).unwrap(),
                )))
            } else {
                Some(Ok(Value::str(
                    super::raku_repr::setbagmix_raku(target).unwrap(),
                )))
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
    })
}
