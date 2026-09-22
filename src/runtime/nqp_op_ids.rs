//! The compile-time registry of `nqp::` VALUE ops: one dense `u16` id per op
//! name, plus which of the six chained dispatch tables owns it.
//!
//! In NQP/Rakudo an `nqp::` value op is a `QAST::Op` node the QAST compiler
//! turns into a single MoarVM instruction; it is not a call and has no name at
//! runtime. mutsu used to leave every one of them an ordinary
//! `OpCode::CallFunc`, so each execution re-derived from the callee STRING
//! what is a fixed property of the call site: strip the `nqp::` prefix, then
//! walk up to six chained `match op { ... }` tables
//! (`call_nqp_interpreter_op` -> `call_nqp_op` -> `call_nqp_op_process` ->
//! `call_nqp_op_text` -> `call_nqp_op_str` -> `call_nqp_op_list`) until one
//! claimed the name. An op late in that chain — `nqp::ordat`, in
//! `JSON::Fast`'s inner scanner loop — paid four failed table walks before
//! reaching its own.
//!
//! [`nqp_op_id`] resolves the name ONCE, in the compiler
//! (`try_compile_nqp_value_op`), and `OpCode::NqpOp` carries the id. At
//! runtime [`nqp_op_table`] sends the id straight to the owning table, so
//! exactly one `match op` runs instead of up to six.
//!
//! **This table is an optimization, never a semantic gate.** A name missing
//! from it simply compiles to the old `CallFunc` path, which reaches the same
//! dispatch chain and the same loud `Unsupported nqp:: op` error; and a name
//! tagged with the WRONG table falls back to the full chain when its table
//! declines it (see `dispatch_nqp_op_by_id`). So adding an op arm without
//! registering it here costs speed, not correctness — which is what the
//! `nqp_op_registry_names_all_dispatch` test pins.

/// Which of the chained `nqp::` dispatch tables implements an op.
///
/// The chain is ordered, and each table's `_` arm falls through to the next,
/// so a tag names the FIRST table that claims the op (no name is claimed by
/// two, pinned by `nqp_op_registry_is_sorted_and_unique`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NqpOpTable {
    /// `call_nqp_interpreter_op` (runtime/nqp_ops_builtin.rs)
    Builtin,
    /// `call_nqp_op` (runtime/nqp_ops.rs)
    Value,
    /// `call_nqp_op_process` (runtime/nqp_ops_process.rs)
    Process,
    /// `call_nqp_op_text` (runtime/nqp_ops_text.rs)
    Text,
    /// `call_nqp_op_str` (runtime/nqp_ops_str.rs)
    Str,
    /// `call_nqp_op_list` (runtime/nqp_ops_list.rs)
    List,
}

/// Every registered op, as `(name without the `nqp::` prefix, owning table)`.
///
/// SORTED BY NAME — [`nqp_op_id`] binary-searches it, and an id IS an index
/// into it. Ids are therefore not stable across edits to this list; nothing
/// persists one (bytecode is compiled per run), but do not write one down.
static NQP_OPS: [(&str, NqpOpTable); 173] = [
    ("abs_i", NqpOpTable::Value),
    ("abs_n", NqpOpTable::Value),
    ("add_I", NqpOpTable::Value),
    ("add_i", NqpOpTable::Value),
    ("add_n", NqpOpTable::Value),
    ("atkey", NqpOpTable::Builtin),
    ("atpos", NqpOpTable::Builtin),
    ("atpos_i", NqpOpTable::Value),
    ("atpos_n", NqpOpTable::Value),
    ("atpos_s", NqpOpTable::Text),
    ("backtrace", NqpOpTable::Builtin),
    ("bindattr", NqpOpTable::Builtin),
    ("bindattr_i", NqpOpTable::Builtin),
    ("bindattr_n", NqpOpTable::Builtin),
    ("bindattr_s", NqpOpTable::Builtin),
    ("bindhllsym", NqpOpTable::Builtin),
    ("bindkey", NqpOpTable::Str),
    ("bindpos", NqpOpTable::List),
    ("bindpos_i", NqpOpTable::Value),
    ("bindpos_n", NqpOpTable::Value),
    ("bindpos_s", NqpOpTable::Text),
    ("bitand_i", NqpOpTable::Value),
    ("bitneg_i", NqpOpTable::Value),
    ("bitor_i", NqpOpTable::Value),
    ("bitshiftl_i", NqpOpTable::Value),
    ("bitshiftr_i", NqpOpTable::Value),
    ("bitxor_i", NqpOpTable::Value),
    ("box_i", NqpOpTable::Builtin),
    ("box_s", NqpOpTable::Text),
    ("can", NqpOpTable::Process),
    ("chars", NqpOpTable::Value),
    ("chr", NqpOpTable::List),
    ("clone", NqpOpTable::Str),
    ("clone_nd", NqpOpTable::Str),
    ("closedir", NqpOpTable::Value),
    ("closefh", NqpOpTable::Value),
    ("cmp_i", NqpOpTable::Value),
    ("cmp_n", NqpOpTable::Value),
    ("cmp_s", NqpOpTable::Value),
    ("coerce_is", NqpOpTable::Value),
    ("coerce_si", NqpOpTable::Value),
    ("concat", NqpOpTable::Str),
    ("create", NqpOpTable::Builtin),
    ("decode", NqpOpTable::Value),
    ("decont", NqpOpTable::Builtin),
    ("defined", NqpOpTable::Process),
    ("deletekey", NqpOpTable::Str),
    ("div_i", NqpOpTable::Value),
    ("div_n", NqpOpTable::Value),
    ("elems", NqpOpTable::Value),
    ("eqaddr", NqpOpTable::Process),
    ("eqat", NqpOpTable::Text),
    ("eqatic", NqpOpTable::Text),
    ("existskey", NqpOpTable::Str),
    ("fileislink", NqpOpTable::Value),
    ("filereadable", NqpOpTable::Value),
    ("findcclass", NqpOpTable::Text),
    ("findnotcclass", NqpOpTable::Text),
    ("flip", NqpOpTable::Str),
    ("getattr", NqpOpTable::Builtin),
    ("getattr_i", NqpOpTable::Builtin),
    ("getattr_n", NqpOpTable::Builtin),
    ("getattr_s", NqpOpTable::Builtin),
    ("gethllsym", NqpOpTable::Builtin),
    ("gethostname", NqpOpTable::Builtin),
    ("getlexdyn", NqpOpTable::Builtin),
    ("getmessage", NqpOpTable::Builtin),
    ("getpayload", NqpOpTable::Builtin),
    ("getstderr", NqpOpTable::Process),
    ("getstdin", NqpOpTable::Process),
    ("getstdout", NqpOpTable::Process),
    ("getuniprop_int", NqpOpTable::Text),
    ("getuniprop_str", NqpOpTable::Text),
    ("hash", NqpOpTable::List),
    ("hllbool", NqpOpTable::Text),
    ("hllize", NqpOpTable::Process),
    ("ifnull", NqpOpTable::Builtin),
    ("index", NqpOpTable::Str),
    ("indexic", NqpOpTable::Str),
    ("indexicim", NqpOpTable::Str),
    ("indexim", NqpOpTable::Str),
    ("iscclass", NqpOpTable::Text),
    ("isconcrete", NqpOpTable::Process),
    ("isconcrete_nd", NqpOpTable::Process),
    ("iseq_i", NqpOpTable::Value),
    ("iseq_n", NqpOpTable::Value),
    ("iseq_s", NqpOpTable::Value),
    ("isge_i", NqpOpTable::Value),
    ("isge_n", NqpOpTable::Value),
    ("isgt_i", NqpOpTable::Value),
    ("isgt_n", NqpOpTable::Value),
    ("isle_i", NqpOpTable::Value),
    ("isle_n", NqpOpTable::Value),
    ("islist", NqpOpTable::Process),
    ("islt_i", NqpOpTable::Value),
    ("islt_n", NqpOpTable::Value),
    ("isnanorinf", NqpOpTable::Value),
    ("isne_i", NqpOpTable::Value),
    ("isne_n", NqpOpTable::Value),
    ("isne_s", NqpOpTable::Value),
    ("isnull", NqpOpTable::Text),
    ("isnull_s", NqpOpTable::Text),
    ("istrue", NqpOpTable::Process),
    ("istype", NqpOpTable::Value),
    ("istype_nd", NqpOpTable::Value),
    ("join", NqpOpTable::Process),
    ("lc", NqpOpTable::Str),
    ("list", NqpOpTable::Process),
    ("list_i", NqpOpTable::Text),
    ("list_n", NqpOpTable::Text),
    ("list_s", NqpOpTable::Text),
    ("lock", NqpOpTable::Process),
    ("mod_i", NqpOpTable::Text),
    ("mul_i", NqpOpTable::Value),
    ("mul_n", NqpOpTable::Value),
    ("neg_i", NqpOpTable::Value),
    ("neg_n", NqpOpTable::Value),
    ("nextfiledir", NqpOpTable::Value),
    ("not_i", NqpOpTable::Value),
    ("null", NqpOpTable::Text),
    ("null_s", NqpOpTable::Text),
    ("objprimspec", NqpOpTable::Value),
    ("open", NqpOpTable::Value),
    ("opendir", NqpOpTable::Value),
    ("ordat", NqpOpTable::Builtin),
    ("p6bindattrinvres", NqpOpTable::List),
    ("p6box_i", NqpOpTable::Value),
    ("p6box_n", NqpOpTable::Value),
    ("p6box_s", NqpOpTable::Value),
    ("p6scalarwithvalue", NqpOpTable::List),
    ("pop", NqpOpTable::List),
    ("pop_i", NqpOpTable::List),
    ("pop_n", NqpOpTable::List),
    ("pop_s", NqpOpTable::List),
    ("push", NqpOpTable::List),
    ("push_i", NqpOpTable::Text),
    ("push_n", NqpOpTable::Text),
    ("push_s", NqpOpTable::Text),
    ("radix", NqpOpTable::Value),
    ("readfh", NqpOpTable::Value),
    ("readint", NqpOpTable::Value),
    ("readnum", NqpOpTable::Value),
    ("readuint", NqpOpTable::Value),
    ("rindex", NqpOpTable::Str),
    ("setbuffersizefh", NqpOpTable::Process),
    ("setelems", NqpOpTable::Builtin),
    ("sha1", NqpOpTable::Builtin),
    ("shift", NqpOpTable::List),
    ("shift_i", NqpOpTable::List),
    ("shift_n", NqpOpTable::List),
    ("shift_s", NqpOpTable::List),
    ("slice", NqpOpTable::Value),
    ("splice", NqpOpTable::Value),
    ("split", NqpOpTable::Process),
    ("stat", NqpOpTable::Value),
    ("strfromcodes", NqpOpTable::Text),
    ("strtocodes", NqpOpTable::Text),
    ("sub_I", NqpOpTable::Value),
    ("sub_i", NqpOpTable::Value),
    ("sub_n", NqpOpTable::Value),
    ("substr", NqpOpTable::Str),
    ("time", NqpOpTable::Process),
    ("uc", NqpOpTable::Str),
    ("unbox_i", NqpOpTable::Builtin),
    ("unbox_s", NqpOpTable::Value),
    ("unipropcode", NqpOpTable::Text),
    ("unlock", NqpOpTable::Process),
    ("unshift", NqpOpTable::Process),
    ("what", NqpOpTable::Process),
    ("writeint", NqpOpTable::Value),
    ("writenum", NqpOpTable::Value),
    ("writeuint", NqpOpTable::Value),
    ("x", NqpOpTable::Str),
];

/// The dense id of an `nqp::` op name (given WITHOUT the `nqp::` prefix), or
/// `None` when this table does not know it — in which case the call site keeps
/// the general `CallFunc` path.
pub(crate) fn nqp_op_id(name: &str) -> Option<u16> {
    NQP_OPS
        .binary_search_by_key(&name, |(n, _)| *n)
        .ok()
        .map(|i| i as u16)
}

/// The op name an id stands for, for error messages and for the table
/// functions, which still match on `&str` internally.
pub(crate) fn nqp_op_name(id: u16) -> &'static str {
    NQP_OPS[id as usize].0
}

/// The table that claims this id.
pub(crate) fn nqp_op_table(id: u16) -> NqpOpTable {
    NQP_OPS[id as usize].1
}

/// How many ids there are. [`crate::runtime::nqp_pure`] builds a table
/// index-parallel to this one, derived from the NAMES here rather than from
/// hardcoded ids (which are not stable across edits to `NQP_OPS`).
pub(crate) fn nqp_op_count() -> usize {
    NQP_OPS.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nqp_op_registry_is_sorted_and_unique() {
        for pair in NQP_OPS.windows(2) {
            assert!(
                pair[0].0 < pair[1].0,
                "NQP_OPS must be sorted and duplicate-free for the binary search: \
                 {:?} is not before {:?}",
                pair[0].0,
                pair[1].0
            );
        }
    }

    #[test]
    fn nqp_op_id_round_trips() {
        for (i, (name, table)) in NQP_OPS.iter().enumerate() {
            assert_eq!(nqp_op_id(name), Some(i as u16), "id of {name}");
            assert_eq!(nqp_op_name(i as u16), *name);
            assert_eq!(nqp_op_table(i as u16), *table);
        }
        assert_eq!(nqp_op_id("no_such_nqp_op"), None);
        // The control-flow forms are compiled as special forms and must never
        // reach the value-op path.
        for form in ["if", "while", "until", "stmts", "handle"] {
            assert_eq!(nqp_op_id(form), None, "{form} is a control-flow form");
        }
    }
}
