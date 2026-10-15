use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(crate) fn native_method_2arg(
    target: &Value,
    method_sym: Symbol,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this arity).
    let target = target.descalarize();
    // Cost: O(n), n = bytes of the invocant.
    if method == "naive-word-wrapper" {
        return crate::builtins::naive_word_wrapper::native_naive_word_wrapper(
            target,
            &[arg1, arg2],
        );
    }
    // Cost: O(n), n = chars in a string bound/value or the error label.
    // The `Range.in-range` row's implementation (`method_table::range`).
    if method == "in-range" {
        return crate::builtins::method_table::range::in_range_what(
            target,
            &[arg1.clone(), arg2.clone()],
        );
    }
    // `Backtrace`: the rows' handlers (ADR-11276 §9.35).
    if let Some(answer) = crate::builtins::method_table::backtrace::answer(
        target,
        method,
        &[arg1.clone(), arg2.clone()],
    ) {
        return Some(answer);
    }
    // `.substr-eq($needle, $pos)` with a plain non-negative Int position is a
    // pure substring comparison on a Str receiver. Whatever / negative /
    // out-of-range positions and the case-/mark-insensitive named-arg forms
    // (`:i`/`:m`, which arrive as an extra Pair argument) keep the interpreter's
    // position resolution + Failure semantics (runtime/methods_string.rs).
    // Cost: O(m) amortized, m = chars of the needle, once the invocant's
    // grapheme index is cached (`$pos` is resolved through it).
    if method == "substr-eq"
        && let ValueView::Str(_) = target.view()
    {
        if let ValueView::Package(type_name) = arg1.view() {
            return Some(Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            ))));
        }
        // The `Str.substr-eq` row's handler (`method_table::str_args`).
        return crate::builtins::method_table::str_args::substr_eq(
            target,
            &[arg1.clone(), arg2.descalarize().clone()],
        );
    }

    if method == "split" {
        if let ValueView::Instance { class_name, .. } = target.view()
            && (class_name == "Supply"
                || class_name == "IO::Handle"
                || class_name == "IO::Pipe"
                || class_name == "IO::CatHandle")
        {
            return None;
        }
        if let ValueView::Instance { class_name, .. } = target.view()
            && crate::value::DispatchShape::from_instance_class(class_name)
                == Some(crate::value::DispatchShape::IoPath)
        {
            return None;
        }
        if let ValueView::Package(name) = target.view()
            && name.as_str().starts_with("IO::Spec")
        {
            return None;
        }
        return crate::builtins::split::native_split_method(target, &[arg1.clone(), arg2.clone()]);
    }

    if method == "comb" {
        // Supply/IO targets keep their interpreter comb semantics.
        if let ValueView::Instance { class_name, .. } = target.view()
            && (class_name == "Supply"
                || class_name == "IO::Handle"
                || class_name == "IO::Path"
                || class_name == "IO::Pipe"
                || class_name == "IO::CatHandle")
        {
            return None;
        }
        // `.comb(matcher, limit)`: pure Int/Str split shared with the
        // interpreter; Regex/Sub/bare matchers return None -> interpreter.
        return crate::builtins::comb::native_comb_method(target, &[arg1.clone(), arg2.clone()]);
    }

    // `fmt`, `substr` and `base` are rows that take any plain argument, and
    // `unimatch` the `Cool` row (ADR-11276 §9.113).
    None
}
