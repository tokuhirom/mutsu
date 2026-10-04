//! The compile-time registry of `nqp::` VALUE ops: one dense `u16` id per op
//! name, plus which of the thirteen chained dispatch tables owns it.
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
//! exactly one `match op` runs instead of up to seven.
//!
//! **This table is an optimization, never a semantic gate.** A name missing
//! from it simply compiles to the old `CallFunc` path, which reaches the same
//! dispatch chain and the same loud `Unsupported nqp:: op` error; and a name
//! tagged with the WRONG table falls back to the full chain when its table
//! declines it (see `dispatch_nqp_op_by_id`). So adding an op arm without
//! registering it here costs speed, not correctness — which is what the
//! `nqp_op_registry_names_all_dispatch` test pins.
//!
//! # Complexity annotations
//!
//! (The shared rules for every annotated family are in
//! `docs/complexity-annotations.md`; this section is the `nqp::` specifics.)
//!
//! Every op arm in the dispatch tables (and each `NqpPure` body) carries one
//! comment line in a single, grep-able form:
//!
//! ```text
//! // Cost: O(n), n = chars of $s.
//! // Cost: O(n), n = chars of $s. MoarVM: O(1) -- see #NNNN.
//! ```
//!
//! * The bound is the op body's own work, per call. The fixed dispatch
//!   overhead every op pays (`exec_nqp_op`'s prologue, the table `match`;
//!   see `nqp_pure`) is a constant and is NOT counted.
//! * Variables name what they measure, per op. The usual letters are
//!   `n` (length of the string / buffer operand, in chars or bytes as
//!   stated), `e` (elements of the array / hash operand), `k` (elements or
//!   chars produced / requested), `m` (needle / separator length).
//! * Converting a `Str` operand with `to_string_value()` copies it, so it is
//!   O(n); cloning a `Value` is a refcount bump, O(1). The string ops share
//!   the `Str` methods' routines (`builtins::str_prim`, ADR-0117) and so
//!   resolve positions through the per-payload `grapheme_index` cache: a hit
//!   is O(1) for a flat ASCII string and O(STRIDE) otherwise, a miss builds
//!   the index in O(n) — "amortized" relies on the hit.
//! * A `MoarVM: O(..)` suffix appears only where mutsu's bound is WORSE than
//!   the one MoarVM gives the same op. Each such gap has a tracking issue,
//!   so `grep -rn 'MoarVM: O(' src/` lists every known complexity deficit.
//! * `scripts/nqp-complexity-check.sh` measures the claims empirically (each
//!   op in a loop at N and 2N; a time ratio near 4 means the loop is
//!   quadratic). When a deficit is fixed, re-run its case there and drop the
//!   `MoarVM:` suffix together with the issue.

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
    /// `call_nqp_op_native` (runtime/nqp_ops_native.rs)
    Native,
    /// `call_nqp_op_p6` (runtime/nqp_ops_p6.rs)
    P6,
    /// `call_nqp_op_sys` (runtime/nqp_ops_sys.rs)
    Sys,
    /// `call_nqp_op_fs` (runtime/nqp_ops_fs.rs)
    Fs,
    /// `call_nqp_op_decoder` (runtime/nqp_ops_decoder.rs)
    Decoder,
    /// `call_nqp_op_capture` (runtime/call_capture.rs)
    Capture,
    /// `call_nqp_op_sc` (runtime/nqp_ops_sc.rs)
    Sc,
}

/// Every registered op, as `(name without the `nqp::` prefix, owning table)`.
///
/// SORTED BY NAME — [`nqp_op_id`] binary-searches it, and an id IS an index
/// into it. Ids are therefore not stable across edits to this list; nothing
/// persists one (bytecode is compiled per run), but do not write one down.
static NQP_OPS: [(&str, NqpOpTable); 472] = [
    ("abs_I", NqpOpTable::Value),
    ("abs_i", NqpOpTable::Value),
    ("abs_n", NqpOpTable::Value),
    ("acos_n", NqpOpTable::Value),
    ("add_I", NqpOpTable::Value),
    ("add_i", NqpOpTable::Value),
    ("add_n", NqpOpTable::Value),
    ("asin_n", NqpOpTable::Value),
    ("atan2_n", NqpOpTable::Value),
    ("atan_n", NqpOpTable::Value),
    ("atkey", NqpOpTable::Builtin),
    ("atpos", NqpOpTable::Builtin),
    ("atpos2d", NqpOpTable::List),
    ("atpos2d_i", NqpOpTable::List),
    ("atpos2d_n", NqpOpTable::List),
    ("atpos2d_s", NqpOpTable::List),
    ("atpos3d", NqpOpTable::List),
    ("atpos3d_i", NqpOpTable::List),
    ("atpos3d_n", NqpOpTable::List),
    ("atpos3d_s", NqpOpTable::List),
    ("atpos_i", NqpOpTable::Value),
    ("atpos_n", NqpOpTable::Value),
    ("atpos_s", NqpOpTable::Text),
    ("atpos_u", NqpOpTable::Native),
    ("atposnd", NqpOpTable::List),
    ("atposnd_i", NqpOpTable::List),
    ("atposnd_n", NqpOpTable::List),
    ("atposnd_s", NqpOpTable::List),
    ("atposref_i", NqpOpTable::Native),
    ("atposref_n", NqpOpTable::Native),
    ("atposref_s", NqpOpTable::Native),
    ("atposref_u", NqpOpTable::Native),
    ("attrinited", NqpOpTable::Builtin),
    ("backendconfig", NqpOpTable::Sys),
    ("backtrace", NqpOpTable::Builtin),
    ("backtracestrings", NqpOpTable::Builtin),
    ("barrierfull", NqpOpTable::Process),
    ("base_I", NqpOpTable::Value),
    ("bindattr", NqpOpTable::Builtin),
    ("bindattr_i", NqpOpTable::Builtin),
    ("bindattr_n", NqpOpTable::Builtin),
    ("bindattr_s", NqpOpTable::Builtin),
    ("bindcomp", NqpOpTable::Builtin),
    ("bindcurhllsym", NqpOpTable::Builtin),
    ("bindhllsym", NqpOpTable::Builtin),
    ("bindkey", NqpOpTable::Str),
    ("bindpos", NqpOpTable::List),
    ("bindpos2d", NqpOpTable::List),
    ("bindpos2d_i", NqpOpTable::List),
    ("bindpos2d_n", NqpOpTable::List),
    ("bindpos2d_s", NqpOpTable::List),
    ("bindpos3d", NqpOpTable::List),
    ("bindpos3d_i", NqpOpTable::List),
    ("bindpos3d_n", NqpOpTable::List),
    ("bindpos3d_s", NqpOpTable::List),
    ("bindpos_i", NqpOpTable::Value),
    ("bindpos_n", NqpOpTable::Value),
    ("bindpos_s", NqpOpTable::Text),
    ("bindpos_u", NqpOpTable::Native),
    ("bindposnd", NqpOpTable::List),
    ("bindposnd_i", NqpOpTable::List),
    ("bindposnd_n", NqpOpTable::List),
    ("bindposnd_s", NqpOpTable::List),
    ("bitand_I", NqpOpTable::Value),
    ("bitand_i", NqpOpTable::Value),
    ("bitand_s", NqpOpTable::Str),
    ("bitneg_I", NqpOpTable::Value),
    ("bitneg_i", NqpOpTable::Value),
    ("bitor_I", NqpOpTable::Value),
    ("bitor_i", NqpOpTable::Value),
    ("bitor_s", NqpOpTable::Str),
    ("bitshiftl_I", NqpOpTable::Value),
    ("bitshiftl_i", NqpOpTable::Value),
    ("bitshiftr_I", NqpOpTable::Value),
    ("bitshiftr_i", NqpOpTable::Value),
    ("bitxor_I", NqpOpTable::Value),
    ("bitxor_i", NqpOpTable::Value),
    ("bitxor_s", NqpOpTable::Str),
    ("bool_I", NqpOpTable::Native),
    ("box_i", NqpOpTable::Builtin),
    ("box_n", NqpOpTable::Native),
    ("box_s", NqpOpTable::Text),
    ("box_u", NqpOpTable::Native),
    ("buildnativecall", NqpOpTable::Native),
    ("can", NqpOpTable::Process),
    ("captureexistsnamed", NqpOpTable::Capture),
    ("capturehasnameds", NqpOpTable::Capture),
    ("capturenamedshash", NqpOpTable::Capture),
    ("captureposarg", NqpOpTable::Capture),
    ("captureposarg_i", NqpOpTable::Capture),
    ("captureposarg_n", NqpOpTable::Capture),
    ("captureposarg_s", NqpOpTable::Capture),
    ("captureposelems", NqpOpTable::Capture),
    ("captureposprimspec", NqpOpTable::Capture),
    ("ceil_n", NqpOpTable::Value),
    ("chars", NqpOpTable::Value),
    ("chdir", NqpOpTable::Fs),
    ("chmod", NqpOpTable::Fs),
    ("chown", NqpOpTable::Fs),
    ("chr", NqpOpTable::List),
    ("clone", NqpOpTable::Str),
    ("clone_nd", NqpOpTable::Str),
    ("closedir", NqpOpTable::Value),
    ("closefh", NqpOpTable::Value),
    ("cmp_I", NqpOpTable::Value),
    ("cmp_i", NqpOpTable::Value),
    ("cmp_n", NqpOpTable::Value),
    ("cmp_s", NqpOpTable::Value),
    ("cmp_u", NqpOpTable::Str),
    ("codepointfromname", NqpOpTable::Text),
    ("codes", NqpOpTable::Str),
    ("coerce_in", NqpOpTable::Native),
    ("coerce_is", NqpOpTable::Value),
    ("coerce_iu", NqpOpTable::Native),
    ("coerce_ni", NqpOpTable::Native),
    ("coerce_ns", NqpOpTable::Native),
    ("coerce_si", NqpOpTable::Value),
    ("coerce_ui", NqpOpTable::Native),
    ("coerce_us", NqpOpTable::Native),
    ("concat", NqpOpTable::Str),
    ("copy", NqpOpTable::Fs),
    ("cos_n", NqpOpTable::Value),
    ("cosh_n", NqpOpTable::Value),
    ("cpucores", NqpOpTable::Sys),
    ("create", NqpOpTable::Builtin),
    ("createsc", NqpOpTable::Sc),
    ("ctx", NqpOpTable::Builtin),
    ("ctxcaller", NqpOpTable::Builtin),
    ("ctxlexpad", NqpOpTable::Builtin),
    ("currentthread", NqpOpTable::Process),
    ("cwd", NqpOpTable::Fs),
    ("decode", NqpOpTable::Value),
    ("decodelocaltime", NqpOpTable::Sys),
    ("decoderaddbytes", NqpOpTable::Decoder),
    ("decoderbytesavailable", NqpOpTable::Decoder),
    ("decoderconfigure", NqpOpTable::Decoder),
    ("decoderempty", NqpOpTable::Decoder),
    ("decodersetlineseps", NqpOpTable::Decoder),
    ("decodertakeallchars", NqpOpTable::Decoder),
    ("decodertakeavailablechars", NqpOpTable::Decoder),
    ("decodertakebytes", NqpOpTable::Decoder),
    ("decodertakechars", NqpOpTable::Decoder),
    ("decodertakecharseof", NqpOpTable::Decoder),
    ("decodertakeline", NqpOpTable::Decoder),
    ("decodetocodes", NqpOpTable::Str),
    ("decont", NqpOpTable::Builtin),
    ("decont_i", NqpOpTable::Native),
    ("decont_n", NqpOpTable::Native),
    ("decont_s", NqpOpTable::Native),
    ("defined", NqpOpTable::Process),
    ("deletekey", NqpOpTable::Str),
    ("die", NqpOpTable::Builtin),
    ("die_s", NqpOpTable::Builtin),
    ("div_I", NqpOpTable::Value),
    ("div_In", NqpOpTable::Value),
    ("div_i", NqpOpTable::Value),
    ("div_n", NqpOpTable::Value),
    ("elems", NqpOpTable::Value),
    ("encode", NqpOpTable::Str),
    ("encodefromcodes", NqpOpTable::Str),
    ("eoffh", NqpOpTable::Fs),
    ("eqaddr", NqpOpTable::Process),
    ("eqat", NqpOpTable::Text),
    ("eqatic", NqpOpTable::Text),
    ("eqaticim", NqpOpTable::Str),
    ("eqatim", NqpOpTable::Str),
    ("escape", NqpOpTable::Str),
    ("exception", NqpOpTable::Builtin),
    ("execname", NqpOpTable::Sys),
    ("existskey", NqpOpTable::Str),
    ("existspos", NqpOpTable::List),
    ("exit", NqpOpTable::Sys),
    ("exp_n", NqpOpTable::Value),
    ("expmod_I", NqpOpTable::Value),
    ("fc", NqpOpTable::Str),
    ("fileexecutable", NqpOpTable::Fs),
    ("fileislink", NqpOpTable::Value),
    ("filenofh", NqpOpTable::Fs),
    ("filereadable", NqpOpTable::Value),
    ("filewritable", NqpOpTable::Fs),
    ("findcclass", NqpOpTable::Text),
    ("findmethod", NqpOpTable::Process),
    ("findnotcclass", NqpOpTable::Text),
    ("flip", NqpOpTable::Str),
    ("floor_n", NqpOpTable::Value),
    ("flushfh", NqpOpTable::Fs),
    ("force_gc", NqpOpTable::Builtin),
    ("freemem", NqpOpTable::Sys),
    ("freshcoderef", NqpOpTable::Native),
    ("fromI_I", NqpOpTable::Native),
    ("fromnum_I", NqpOpTable::Native),
    ("fromstr_I", NqpOpTable::Native),
    ("gcd_I", NqpOpTable::Value),
    ("gcd_i", NqpOpTable::Value),
    ("getattr", NqpOpTable::Builtin),
    ("getattr_i", NqpOpTable::Builtin),
    ("getattr_n", NqpOpTable::Builtin),
    ("getattr_s", NqpOpTable::Builtin),
    ("getcodename", NqpOpTable::Native),
    ("getcomp", NqpOpTable::Builtin),
    ("getcurhllsym", NqpOpTable::Builtin),
    ("getenvhash", NqpOpTable::Sys),
    ("getextype", NqpOpTable::Builtin),
    ("gethllsym", NqpOpTable::Builtin),
    ("gethostname", NqpOpTable::Builtin),
    ("getlexdyn", NqpOpTable::Builtin),
    ("getmessage", NqpOpTable::Builtin),
    ("getobjsc", NqpOpTable::Sc),
    ("getpayload", NqpOpTable::Builtin),
    ("getpid", NqpOpTable::Sys),
    ("getport", NqpOpTable::Fs),
    ("getppid", NqpOpTable::Sys),
    ("getrusage", NqpOpTable::Process),
    ("getsignals", NqpOpTable::Sys),
    ("getstderr", NqpOpTable::Process),
    ("getstdin", NqpOpTable::Process),
    ("getstdout", NqpOpTable::Process),
    ("getuniname", NqpOpTable::Text),
    ("getuniprop_bool", NqpOpTable::Text),
    ("getuniprop_int", NqpOpTable::Text),
    ("getuniprop_str", NqpOpTable::Text),
    ("hash", NqpOpTable::List),
    ("hasuniprop", NqpOpTable::Text),
    ("hllbool", NqpOpTable::Text),
    ("hllboxtype_i", NqpOpTable::Builtin),
    ("hllboxtype_n", NqpOpTable::Builtin),
    ("hllboxtype_s", NqpOpTable::Builtin),
    ("hllhash", NqpOpTable::Builtin),
    ("hllize", NqpOpTable::Process),
    ("hlllist", NqpOpTable::Builtin),
    ("ifnull", NqpOpTable::Builtin),
    ("index", NqpOpTable::Str),
    ("indexfrom", NqpOpTable::Str),
    ("indexic", NqpOpTable::Str),
    ("indexicim", NqpOpTable::Str),
    ("indexim", NqpOpTable::Str),
    ("indexingoptimized", NqpOpTable::Str),
    ("inf", NqpOpTable::Value),
    ("intify", NqpOpTable::Native),
    ("isbig_I", NqpOpTable::Native),
    ("iscclass", NqpOpTable::Text),
    ("iscoderef", NqpOpTable::Native),
    ("isconcrete", NqpOpTable::Process),
    ("isconcrete_nd", NqpOpTable::Process),
    ("iscont", NqpOpTable::Process),
    ("iseq_I", NqpOpTable::Value),
    ("iseq_i", NqpOpTable::Value),
    ("iseq_n", NqpOpTable::Value),
    ("iseq_s", NqpOpTable::Value),
    ("iseq_u", NqpOpTable::Str),
    ("isfalse", NqpOpTable::Process),
    ("isge_I", NqpOpTable::Value),
    ("isge_i", NqpOpTable::Value),
    ("isge_n", NqpOpTable::Value),
    ("isge_s", NqpOpTable::Str),
    ("isge_u", NqpOpTable::Str),
    ("isgt_I", NqpOpTable::Value),
    ("isgt_i", NqpOpTable::Value),
    ("isgt_n", NqpOpTable::Value),
    ("isgt_s", NqpOpTable::Str),
    ("isgt_u", NqpOpTable::Str),
    ("isinvokable", NqpOpTable::Native),
    ("isle_I", NqpOpTable::Value),
    ("isle_i", NqpOpTable::Value),
    ("isle_n", NqpOpTable::Value),
    ("isle_s", NqpOpTable::Str),
    ("isle_u", NqpOpTable::Str),
    ("islist", NqpOpTable::Process),
    ("islt_I", NqpOpTable::Value),
    ("islt_i", NqpOpTable::Value),
    ("islt_n", NqpOpTable::Value),
    ("islt_s", NqpOpTable::Str),
    ("islt_u", NqpOpTable::Str),
    ("isnanorinf", NqpOpTable::Value),
    ("isne_I", NqpOpTable::Value),
    ("isne_i", NqpOpTable::Value),
    ("isne_n", NqpOpTable::Value),
    ("isne_s", NqpOpTable::Value),
    ("isne_u", NqpOpTable::Str),
    ("isnull", NqpOpTable::Text),
    ("isnull_s", NqpOpTable::Text),
    ("isprime_I", NqpOpTable::Native),
    ("istrue", NqpOpTable::Process),
    ("isttyfh", NqpOpTable::Native),
    ("istype", NqpOpTable::Value),
    ("istype_nd", NqpOpTable::Value),
    ("iterator", NqpOpTable::Text),
    ("iterkey_s", NqpOpTable::Text),
    ("iterval", NqpOpTable::Text),
    ("join", NqpOpTable::Process),
    ("lc", NqpOpTable::Str),
    ("lcm_I", NqpOpTable::Value),
    ("lcm_i", NqpOpTable::Value),
    ("link", NqpOpTable::Fs),
    ("list", NqpOpTable::Process),
    ("list_i", NqpOpTable::Text),
    ("list_n", NqpOpTable::Text),
    ("list_s", NqpOpTable::Text),
    ("lock", NqpOpTable::Process),
    ("log_n", NqpOpTable::Value),
    ("lstat", NqpOpTable::Value),
    ("lstat_time", NqpOpTable::Fs),
    ("markcodestatic", NqpOpTable::Native),
    ("matchuniprop", NqpOpTable::Text),
    ("mkdir", NqpOpTable::Fs),
    ("mod_I", NqpOpTable::Value),
    ("mod_i", NqpOpTable::Text),
    ("mod_n", NqpOpTable::Value),
    ("mul_I", NqpOpTable::Value),
    ("mul_i", NqpOpTable::Value),
    ("mul_n", NqpOpTable::Value),
    ("nan", NqpOpTable::Value),
    ("nativecall", NqpOpTable::Native),
    ("nativecallcast", NqpOpTable::Native),
    ("nativecallglobal", NqpOpTable::Native),
    ("nativecallrefresh", NqpOpTable::Native),
    ("nativecallsizeof", NqpOpTable::Native),
    ("neg_I", NqpOpTable::Value),
    ("neg_i", NqpOpTable::Value),
    ("neg_n", NqpOpTable::Value),
    ("neginf", NqpOpTable::Value),
    ("neverrepossess", NqpOpTable::Native),
    ("newexception", NqpOpTable::Builtin),
    ("newthread", NqpOpTable::Process),
    ("nextfiledir", NqpOpTable::Value),
    ("normalizecodes", NqpOpTable::Str),
    ("not_i", NqpOpTable::Value),
    ("null", NqpOpTable::Text),
    ("null_s", NqpOpTable::Text),
    ("numify", NqpOpTable::Native),
    ("objprimspec", NqpOpTable::Value),
    ("open", NqpOpTable::Value),
    ("opendir", NqpOpTable::Value),
    ("ord", NqpOpTable::Builtin),
    ("ordat", NqpOpTable::Builtin),
    ("ordbaseat", NqpOpTable::Str),
    ("ordfirst", NqpOpTable::Str),
    ("p6bindassert", NqpOpTable::P6),
    ("p6bindattrinvres", NqpOpTable::List),
    ("p6bindcaptosig", NqpOpTable::P6),
    ("p6box", NqpOpTable::P6),
    ("p6box_i", NqpOpTable::Value),
    ("p6box_n", NqpOpTable::Value),
    ("p6box_s", NqpOpTable::Value),
    ("p6capturelex", NqpOpTable::P6),
    ("p6decontrv", NqpOpTable::P6),
    ("p6decontrv_6c", NqpOpTable::P6),
    ("p6definite", NqpOpTable::P6),
    ("p6getouterctx", NqpOpTable::P6),
    ("p6isbindable", NqpOpTable::P6),
    ("p6scalarwithvalue", NqpOpTable::List),
    ("p6setautothreader", NqpOpTable::P6),
    ("p6trialbind", NqpOpTable::P6),
    ("p6typecheckrv", NqpOpTable::P6),
    ("pop", NqpOpTable::List),
    ("pop_i", NqpOpTable::List),
    ("pop_n", NqpOpTable::List),
    ("pop_s", NqpOpTable::List),
    ("popcompsc", NqpOpTable::Sc),
    ("pow_I", NqpOpTable::Value),
    ("pow_i", NqpOpTable::Value),
    ("pow_n", NqpOpTable::Value),
    ("print", NqpOpTable::Fs),
    ("push", NqpOpTable::List),
    ("push_i", NqpOpTable::Text),
    ("push_n", NqpOpTable::Text),
    ("push_s", NqpOpTable::Text),
    ("pushcompsc", NqpOpTable::Sc),
    ("radix", NqpOpTable::Value),
    ("radix_I", NqpOpTable::Str),
    ("rand_I", NqpOpTable::Value),
    ("rand_i", NqpOpTable::Value),
    ("rand_n", NqpOpTable::Value),
    ("readfh", NqpOpTable::Value),
    ("readint", NqpOpTable::Value),
    ("readlink", NqpOpTable::Process),
    ("readnum", NqpOpTable::Value),
    ("readuint", NqpOpTable::Value),
    ("rename", NqpOpTable::Fs),
    ("replace", NqpOpTable::Str),
    ("resume", NqpOpTable::Builtin),
    ("rethrow", NqpOpTable::Builtin),
    ("rindex", NqpOpTable::Str),
    ("rindexfrom", NqpOpTable::Str),
    ("rmdir", NqpOpTable::Fs),
    ("savecapture", NqpOpTable::Capture),
    ("say", NqpOpTable::Fs),
    ("scgetdesc", NqpOpTable::Sc),
    ("scgethandle", NqpOpTable::Sc),
    ("scgetobjidx", NqpOpTable::Sc),
    ("scobjcount", NqpOpTable::Sc),
    ("scsetcode", NqpOpTable::Sc),
    ("scsetdesc", NqpOpTable::Sc),
    ("scsetobj", NqpOpTable::Sc),
    ("seekfh", NqpOpTable::Fs),
    ("setbuffersizefh", NqpOpTable::Process),
    ("setcodename", NqpOpTable::Native),
    ("setdebugtypename", NqpOpTable::Native),
    ("setelems", NqpOpTable::Builtin),
    ("setextype", NqpOpTable::Builtin),
    ("sethllconfig", NqpOpTable::Builtin),
    ("setmessage", NqpOpTable::Builtin),
    ("setobjsc", NqpOpTable::Sc),
    ("setpayload", NqpOpTable::Builtin),
    ("sha1", NqpOpTable::Builtin),
    ("shift", NqpOpTable::List),
    ("shift_i", NqpOpTable::List),
    ("shift_n", NqpOpTable::List),
    ("shift_s", NqpOpTable::List),
    ("sin_n", NqpOpTable::Value),
    ("sinh_n", NqpOpTable::Value),
    ("sleep", NqpOpTable::Sys),
    ("slice", NqpOpTable::Value),
    ("splice", NqpOpTable::Value),
    ("split", NqpOpTable::Process),
    ("sprintf", NqpOpTable::Str),
    ("sprintfaddargumenthandler", NqpOpTable::Str),
    ("sprintfdirectives", NqpOpTable::Str),
    ("sqrt_n", NqpOpTable::Value),
    ("srand", NqpOpTable::Value),
    ("stat", NqpOpTable::Value),
    ("stat_time", NqpOpTable::Fs),
    ("strfromcodes", NqpOpTable::Text),
    ("strfromname", NqpOpTable::Text),
    ("strtocodes", NqpOpTable::Text),
    ("sub_I", NqpOpTable::Value),
    ("sub_i", NqpOpTable::Value),
    ("sub_n", NqpOpTable::Value),
    ("substr", NqpOpTable::Str),
    ("substr_s", NqpOpTable::Str),
    ("symlink", NqpOpTable::Fs),
    ("takeclosure", NqpOpTable::Native),
    ("tan_n", NqpOpTable::Value),
    ("tanh_n", NqpOpTable::Value),
    ("tc", NqpOpTable::Str),
    ("tclc", NqpOpTable::Str),
    ("tellfh", NqpOpTable::Fs),
    ("threadid", NqpOpTable::Process),
    ("threadjoin", NqpOpTable::Process),
    ("threadlockcount", NqpOpTable::Process),
    ("threadrun", NqpOpTable::Process),
    ("threadyield", NqpOpTable::Process),
    ("throw", NqpOpTable::Builtin),
    ("time", NqpOpTable::Process),
    ("tonum_I", NqpOpTable::Native),
    ("tostr_I", NqpOpTable::Native),
    ("totalmem", NqpOpTable::Sys),
    ("tryfindmethod", NqpOpTable::Process),
    ("uc", NqpOpTable::Str),
    ("uname", NqpOpTable::Sys),
    ("unbox_i", NqpOpTable::Builtin),
    ("unbox_n", NqpOpTable::Native),
    ("unbox_s", NqpOpTable::Value),
    ("unbox_u", NqpOpTable::Native),
    ("unicmp_s", NqpOpTable::Str),
    ("unipropcode", NqpOpTable::Text),
    ("unipvalcode", NqpOpTable::Text),
    ("unlink", NqpOpTable::Fs),
    ("unlock", NqpOpTable::Process),
    ("unshift", NqpOpTable::Process),
    ("unshift_i", NqpOpTable::Process),
    ("unshift_n", NqpOpTable::Process),
    ("unshift_s", NqpOpTable::Process),
    ("usecapture", NqpOpTable::Capture),
    ("usecompileehllconfig", NqpOpTable::Builtin),
    ("usecompilerhllconfig", NqpOpTable::Builtin),
    ("what", NqpOpTable::Process),
    ("writefh", NqpOpTable::Fs),
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
        for form in [
            "if",
            "while",
            "until",
            "repeat_while",
            "repeat_until",
            "stmts",
            "handle",
        ] {
            assert_eq!(nqp_op_id(form), None, "{form} is a control-flow form");
        }
    }

    /// Every registered name reaches an op arm: dispatched by its id, it never
    /// falls through to the chain's `Unsupported nqp:: op` error. The op is run
    /// with no arguments, so most arms fail on their arity, and a few panic on
    /// an unchecked `args[0]`; either still means an arm claimed the name.
    #[test]
    fn nqp_op_registry_names_all_dispatch() {
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let mut unclaimed = Vec::new();
        for (i, (name, _)) in NQP_OPS.iter().enumerate() {
            let mut interp = crate::runtime::Interpreter::new();
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                interp.dispatch_nqp_op_by_id(i as u16, &[])
            }));
            if let Ok(Err(e)) = outcome
                && e.message.starts_with("Unsupported nqp:: op")
            {
                unclaimed.push(*name);
            }
        }
        std::panic::set_hook(hook);
        assert!(
            unclaimed.is_empty(),
            "registered nqp:: ops no dispatch table claims: {unclaimed:?}"
        );
    }
}
