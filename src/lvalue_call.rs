//! The write-through calls the front ends lower an assignment to (ADR-17198).
//!
//! A write through a method call, a named routine or a callable is carried as a
//! call whose callee is one of these names, and the compiler, the VM and the
//! runtime builtins recognise it by that name. The names are executable IR, so
//! they are defined once, here, and every producer and consumer spells them
//! through these constants (`make check-frontend-debt` counts a quoted
//! spelling outside this file).
//!
//! The argument layouts, which the consumers rely on:
//!
//! | callee | arguments |
//! | --- | --- |
//! | [`ASSIGN_METHOD`] | invocant, method name, method arguments, value |
//! | [`ASSIGN_NAMED_SUB`] | routine name, call arguments, value |
//! | [`ASSIGN_CALLABLE`] | callable, call arguments, value |
//! | [`INCDEC_NAMED_SUB`] | routine name, call arguments, increment kind |
//! | [`INDEX_ASSIGN_METHOD`] | invocant, method name, method arguments, index, value |
//! | [`INDEX_DELETE_METHOD`] | invocant, method name, method arguments, index |

/// `$obj.attr = $v`, `$obj.method(...) = $v`: the invocant's rw method is
/// called and its container written.
pub(crate) const ASSIGN_METHOD: &str = "__mutsu_assign_method_lvalue";
/// `f(...) = $v`: the rw routine `f` returns the container written.
pub(crate) const ASSIGN_NAMED_SUB: &str = "__mutsu_assign_named_sub_lvalue";
/// `$code(...) = $v`, `(...) = ...` over a literal list of lvalues, a `my`
/// declaration assigned to.
pub(crate) const ASSIGN_CALLABLE: &str = "__mutsu_assign_callable_lvalue";
/// `f(...)++` and `--f(...)`.
pub(crate) const INCDEC_NAMED_SUB: &str = "__mutsu_incdec_named_sub_lvalue";
/// `$obj.method(...)[$i] = $v`.
pub(crate) const INDEX_ASSIGN_METHOD: &str = "__mutsu_index_assign_method_lvalue";
/// `$obj.method(...)[$i]:delete`.
pub(crate) const INDEX_DELETE_METHOD: &str = "__mutsu_index_delete_method_lvalue";

#[cfg(test)]
mod tests {
    use super::*;

    const ALL: [&str; 6] = [
        ASSIGN_METHOD,
        ASSIGN_NAMED_SUB,
        ASSIGN_CALLABLE,
        INCDEC_NAMED_SUB,
        INDEX_ASSIGN_METHOD,
        INDEX_DELETE_METHOD,
    ];

    #[test]
    fn the_spellings_are_distinct_reserved_names() {
        for (i, a) in ALL.iter().enumerate() {
            assert!(a.starts_with("__mutsu_") && a.ends_with("_lvalue"), "{a}");
            for b in &ALL[i + 1..] {
                assert_ne!(a, b);
            }
        }
    }

    #[test]
    fn the_runtime_dispatches_the_three_assign_forms() {
        // The builtin table and the producers meet on these exact strings.
        assert_eq!(ASSIGN_METHOD, "__mutsu_assign_method_lvalue");
        assert_eq!(ASSIGN_NAMED_SUB, "__mutsu_assign_named_sub_lvalue");
        assert_eq!(ASSIGN_CALLABLE, "__mutsu_assign_callable_lvalue");
    }
}
