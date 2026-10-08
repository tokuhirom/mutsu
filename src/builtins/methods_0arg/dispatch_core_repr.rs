/// Representation methods: gist, raku, perl
use crate::value::{RuntimeError, Value, ValueView};

use super::raku_repr::{promise_raku_repr, raku_value};
use crate::value::types::is_stash_class_name;

/// The scalar rendering rows' handlers: `gist` for `gist`, `raku` for
/// `raku`/`perl`. The table and this cascade share them.
fn render(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::method_table::scalars::render;
    if method == "gist" {
        render::gist(target, &[])
    } else {
        render::raku(target, &[])
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if !matches!(method, "gist" | "raku" | "perl") {
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
        ValueView::Bool(_) => render(target, method),
        // raku/perl and gist of the Nil value are all "Nil". (Uninitialized
        // variables hold the Any type object since PLAN 8.5, so a runtime
        // Nil here is a genuine Nil, not an uninit placeholder.)
        ValueView::Nil => Some(Ok(Value::str_from("Nil"))),
        ValueView::FatRat(n, d) => {
            if d == 0 && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::rational_to_str_divide_by_zero(
                    Value::int(n),
                )))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                Some(Ok(Value::str(format!("FatRat.new({}, {})", n, d))))
            }
        }
        ValueView::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::rational_to_str_divide_by_zero(
                    Value::from_bigint(n.clone()),
                )))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else if target.is_bigfatrat() {
                Some(Ok(Value::str(format!("FatRat.new({}, {})", n, d))))
            } else {
                Some(Ok(Value::str(raku_value(target))))
            }
        }
        ValueView::Rat(n, 0) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(format!("<{}/0>", n))))
            } else {
                Some(Err(RuntimeError::rational_to_str_divide_by_zero(
                    Value::int(n),
                )))
            }
        }
        ValueView::Rat(..) => render(target, method),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Signature" => {
            let attr_key = if method == "gist" { "gist" } else { "raku" };
            Some(Ok(attributes
                .as_map()
                .get(attr_key)
                .cloned()
                .unwrap_or_else(|| Value::str(format!("{}()", class_name)))))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Parameter" && matches!(method, "raku" | "perl" | "gist") => {
            Some(Ok(Value::str(crate::value::signature::parameter_to_raku(
                &(attributes).as_map(),
            ))))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            let msg = attributes
                .as_map()
                .get("exception")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Failed".to_string());
            if method == "gist" {
                let gist = if target.is_failure_handled() {
                    format!("(HANDLED) {}", msg)
                } else {
                    msg
                };
                Some(Ok(Value::str(gist)))
            } else if method == "raku" || method == "perl" {
                let raku_str = if target.is_failure_handled() {
                    // For handled Failures, produce an expression that when
                    // EVALed creates a handled Failure, preserving the flag.
                    format!("do {{ my $f = Failure.new(\"{}\"); $f.Bool; $f }}", msg)
                } else {
                    format!("Failure.new(\"{}\")", msg)
                };
                Some(Ok(Value::str(raku_str)))
            } else {
                // Str, Numeric, Int, etc. -- using a Failure in a value
                // context throws the wrapped exception.
                if let Some(ex) = attributes.as_map().get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new(msg)))
                }
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
        // A buffer's rendering is the `Blob`/`Buf` rows' (`method_table::blob`).
        ValueView::Instance { class_name, .. }
            if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) =>
        {
            crate::builtins::method_table::blob::render(
                target,
                method == "raku" || method == "perl",
            )
        }
        // The renderers the quant hashes' rows share. The rows decline an
        // element that may carry a user `gist`/`raku`; this arm renders it
        // with the default form, as it always did.
        ValueView::Bag(..) | ValueView::Set(..) | ValueView::Mix(..) => {
            if method == "gist" {
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
        ValueView::Version {
            parts,
            plus,
            minus,
            text,
        } => {
            let s = text
                .map(str::to_string)
                .unwrap_or_else(|| Value::version_parts_to_string(parts));
            let suffix = if plus {
                "+"
            } else if minus {
                "-"
            } else {
                ""
            };
            let full = format!("{}{}", s, suffix);
            // .gist always keeps the `v` prefix (`vTrue`); only .raku/.perl
            // switch to the constructor form for a non-literal version.
            if method == "gist" {
                Some(Ok(Value::str(format!("v{}", full))))
            } else {
                Some(Ok(Value::str(super::raku_repr::version_raku_repr(&full))))
            }
        }
        ValueView::Str(_) => render(target, method),
        ValueView::Array(_, kind) if method == "raku" || method == "perl" => {
            if kind == crate::value::ArrayKind::Lazy {
                Some(Ok(Value::str_from("[...]")))
            } else {
                Some(Ok(Value::str(raku_value(target))))
            }
        }
        ValueView::Seq(_) if method == "raku" || method == "perl" => {
            Some(Ok(Value::str(raku_value(target))))
        }
        // Delegate to the single Slip renderer in `raku_repr` rather than
        // repeating it: this arm used to carry its own copy of the `Empty` /
        // trailing-comma rules, so the `$`-itemization the nested renderer
        // learned (`my $x = slip(5, 6); $x.raku` is `$(slip(5, 6))`) never
        // reached a top-level `.raku` call.
        ValueView::Slip(_) if method == "raku" || method == "perl" => {
            Some(Ok(Value::str(raku_value(target))))
        }
        ValueView::Junction { .. } if method == "raku" || method == "perl" => None,
        // A WhateverCode (`*+1`, `*.abs`) renders as `WhateverCode.new` for
        // `.gist`/`.raku`/`.perl`, not the generic closure form.
        ValueView::Sub(data)
            if (method == "raku" || method == "perl" || method == "gist")
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
        ValueView::Pair(..) | ValueView::ValuePair(..) => Some(Ok(Value::str(raku_value(target)))),
        ValueView::BigInt(i) => {
            // A BigInt is an integer (its `.^name` is `Int`); `.raku` must render
            // the plain integer, not a float (`100000000000000000000`, not
            // `100000000000000000000.0`), so it round-trips as an Int.
            Some(Ok(Value::str(i.to_string())))
        }
        ValueView::Int(i) => Some(Ok(Value::str(format!("{}", i)))),
        ValueView::Num(_f) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist == Str for a Num: use the canonical Num→Str formatter,
                // which renders Inf/-Inf/NaN with their proper casing and applies
                // scientific notation for very large / small magnitudes. The raw
                // `format!("{}", f)` produced Rust's lowercase `inf`/`-inf` and
                // never switched to scientific (e.g. `1e20.gist` was the full
                // 21-digit integer instead of `1e+20`).
                Some(Ok(Value::str(target.to_string_value())))
            }
        }
        ValueView::Complex(r, i) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(format!(
                    "<{}>",
                    crate::value::format_complex(r, i)
                ))))
            } else {
                Some(Ok(Value::str(crate::value::format_complex(r, i))))
            }
        }
        // Delegate to raku_value which has cycle detection for self-referencing
        // hashes (e.g. %h<b> = %h).
        ValueView::Hash(..) => Some(Ok(Value::str(raku_value(target)))),
        _ if target.is_range() && (method == "gist" || method == "raku" || method == "perl") => {
            crate::builtins::method_table::collection_render::range_render(target, &[])
        }
        // A genuinely-lazy (infinite) list renders a `(...)`/`[...]`/`...`
        // placeholder rather than materializing — e.g. `[2,3].roll(*).gist`
        // (raku: `(...)`). Finite/`cat_pull` lazy lists fall through to force.
        ValueView::LazyList(ll)
            if (method == "gist" || method == "raku" || method == "perl")
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
        _ => Some(Ok(Value::str(target.to_string_value()))),
    })
}
