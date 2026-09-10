//! Env-gated dump of what the JIT actually emitted (`MUTSU_JIT_DUMP`).
//!
//! The generated bodies are written to anonymous JIT memory at runtime, so a
//! profiler has no symbol for them: on `bench-fib` the largest generated body
//! shows up in `callgrind` only as `???:0x…` at 13.8% of the run
//! ([#7737](https://github.com/tokuhirom/mutsu/issues/7737)). Guessing at the
//! cost from the interpreter side has repeatedly failed; this module makes the
//! emitted code readable instead.
//!
//! Set `MUTSU_JIT_DUMP` to one of:
//!
//! - `ops` — the opcode range that was accepted, plus the finalized code
//!   address and length (enough to correlate a profiler's bare `0x…` symbol
//!   with a chunk);
//! - `clif` — the above plus the Cranelift IR handed to the backend;
//! - `asm` — the above plus the backend's own disassembly of the emitted
//!   machine code, with every helper-shim call address annotated by name;
//! - `all` / `1` — `clif` and `asm` together.
//!
//! Output goes to stderr, or to the file named by `MUTSU_JIT_DUMP_FILE`.
//! Everything here runs once per compiled chunk, on the compilation path
//! only; when the variable is unset the whole module is one `OnceLock` read.

use super::*;
use std::io::Write as _;

/// What `MUTSU_JIT_DUMP` asked for. `Off` short-circuits every call site.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum DumpMode {
    Off,
    Ops,
    Clif,
    Asm,
    All,
}

impl DumpMode {
    pub(super) fn on(self) -> bool {
        self != DumpMode::Off
    }
    fn asm(self) -> bool {
        matches!(self, DumpMode::Asm | DumpMode::All)
    }
    fn clif(self) -> bool {
        matches!(self, DumpMode::Clif | DumpMode::All)
    }
}

/// Resolved once from `MUTSU_JIT_DUMP`; an unrecognized value warns and is
/// treated as `all` (a typo should not silently produce no output when the
/// user clearly asked for a dump).
pub(super) fn mode() -> DumpMode {
    static MODE: std::sync::OnceLock<DumpMode> = std::sync::OnceLock::new();
    *MODE.get_or_init(|| match std::env::var("MUTSU_JIT_DUMP").ok().as_deref() {
        None | Some("") | Some("0") | Some("off") => DumpMode::Off,
        Some("ops") => DumpMode::Ops,
        Some("clif") => DumpMode::Clif,
        Some("asm") => DumpMode::Asm,
        Some("all") | Some("1") => DumpMode::All,
        Some(other) => {
            eprintln!("[mutsu jit] warning: unrecognized MUTSU_JIT_DUMP={other:?}, using \"all\"");
            DumpMode::All
        }
    })
}

/// Append `text` to the dump sink (`MUTSU_JIT_DUMP_FILE`, else stderr).
fn emit(text: &str) {
    match std::env::var("MUTSU_JIT_DUMP_FILE").ok() {
        Some(path) => {
            if let Ok(mut f) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
            {
                let _ = f.write_all(text.as_bytes());
                return;
            }
            eprint!("{text}");
        }
        None => eprint!("{text}"),
    }
}

/// Every shim the emitter can plant as a raw call target, so an address
/// constant in the disassembly can be shown as a name. Built once; the list
/// is small enough that a linear scan per address is irrelevant next to the
/// cost of formatting the disassembly at all.
fn helper_names() -> &'static [(usize, &'static str)] {
    use super::vm_jit_helpers as h;
    static NAMES: std::sync::OnceLock<Vec<(usize, &'static str)>> = std::sync::OnceLock::new();
    NAMES.get_or_init(|| {
        macro_rules! addr {
            ($f:expr) => {
                $f as *const () as usize
            };
        }
        vec![
            (addr!(h::load_const), "load_const"),
            (addr!(h::containerize_pair), "containerize_pair"),
            (addr!(h::safepoint), "safepoint"),
            (addr!(h::mark_failure_top), "mark_failure_top"),
            (addr!(h::get_local), "get_local"),
            (addr!(h::meta_assign_identity), "meta_assign_identity"),
            (
                addr!(h::meta_assign_identity_fallible),
                "meta_assign_identity_fallible",
            ),
            (addr!(h::set_local), "set_local"),
            (addr!(h::set_local_decl), "set_local_decl"),
            (addr!(h::step), "step"),
            (addr!(h::jump_if_false_cond), "jump_if_false_cond"),
            (addr!(h::jump_if_true_cond), "jump_if_true_cond"),
            (addr!(h::jump_if_not_nil_cond), "jump_if_not_nil_cond"),
            (
                addr!(h::state_var_init_guard_cond),
                "state_var_init_guard_cond",
            ),
            (addr!(h::ret), "ret"),
            (addr!(h::call_method), "call_method"),
            (addr!(h::call_method_mut), "call_method_mut"),
            (addr!(h::call_func), "call_func"),
            (addr!(h::add), "add"),
            (addr!(h::sub), "sub"),
            (addr!(h::mul), "mul"),
            (addr!(h::div), "div"),
            (addr!(h::modulo), "modulo"),
            (addr!(h::int_div), "int_div"),
            (addr!(h::int_mod), "int_mod"),
            (addr!(h::pow), "pow"),
            (addr!(h::negate), "negate"),
            (addr!(h::num_lt), "num_lt"),
            (addr!(h::num_le), "num_le"),
            (addr!(h::num_gt), "num_gt"),
            (addr!(h::num_ge), "num_ge"),
            (addr!(h::num_eq), "num_eq"),
            (addr!(h::num_ne), "num_ne"),
            (addr!(h::concat), "concat"),
            (addr!(h::str_eq), "str_eq"),
            (addr!(h::str_ne), "str_ne"),
            (addr!(h::bit_and), "bit_and"),
            (addr!(h::bit_or), "bit_or"),
            (addr!(h::bit_xor), "bit_xor"),
            (addr!(h::bit_shift_left), "bit_shift_left"),
            (addr!(h::bit_shift_right), "bit_shift_right"),
            (addr!(h::int_bit_neg), "int_bit_neg"),
        ]
    })
}

/// Annotate a disassembly line carrying a raw helper address with the shim's
/// name, so `movabs $0x5581…, %r10` reads as a call to `call_func`.
fn annotate(line: &str) -> String {
    let bytes = line.as_bytes();
    let mut found: Option<&'static str> = None;
    let mut i = 0;
    while i + 2 < bytes.len() {
        if bytes[i] == b'0' && bytes[i + 1] == b'x' {
            let start = i + 2;
            let mut end = start;
            while end < bytes.len() && (bytes[end] as char).is_ascii_hexdigit() {
                end += 1;
            }
            if end > start
                && let Ok(v) = usize::from_str_radix(&line[start..end], 16)
                && let Some((_, name)) = helper_names().iter().find(|(a, _)| *a == v)
            {
                found = Some(name);
                break;
            }
            i = end;
            continue;
        }
        i += 1;
    }
    match found {
        Some(name) => format!("{line}    ; -> {name}\n"),
        None => format!("{line}\n"),
    }
}

/// Header + accepted opcode range. Printed before codegen so a chunk that
/// then fails to compile is still identified.
fn dump_ops(name: &str, code: &CompiledCode, start: usize, end: usize) {
    let mut out = String::new();
    out.push_str(&format!(
        "\n=== mutsu jit: {name} ops[{start}..{end}] locals={:?} ===\n",
        code.locals
    ));
    for (i, op) in code.ops[start..end].iter().enumerate() {
        out.push_str(&format!("  {:>4}  {:?}\n", start + i, op));
    }
    emit(&out);
}

/// The Cranelift IR handed to the backend.
fn dump_clif(name: &str, func: &cranelift_codegen::ir::Function) {
    if !mode().clif() {
        return;
    }
    emit(&format!("\n--- {name} clif ---\n{}\n", func.display()));
}

/// The backend's disassembly, the emitted code size, and the per-opcode byte
/// spans (`start end op_index`, function-relative). The spans are what turns a
/// flat profile of the body into a per-opcode cost table: every emitted
/// instruction was tagged with the index of the opcode it came from.
fn dump_code(name: &str, vcode: Option<&str>, len: usize, spans: &[(u32, u32, u32)]) {
    let mut out = String::new();
    out.push_str(&format!("\n--- {name} code ({len} bytes) ---\n"));
    for (start, end, op) in spans {
        out.push_str(&format!("  span {start:#06x}..{end:#06x} op {op}\n"));
    }
    if let Some(v) = vcode {
        for line in v.lines() {
            out.push_str(&annotate(line));
        }
    }
    emit(&out);
}

/// The finalized entry address — the number that lets a profiler's bare
/// `???:0x…` symbol be matched back to a chunk. When `MUTSU_JIT_DUMP_BIN`
/// names a directory, the finalized bytes are also written there as
/// `<name>.bin`, so the body can be disassembled at its real load address
/// (`objdump -D -b binary -m i386:x86-64 --adjust-vma=<entry>`) and lined up
/// with a `callgrind --dump-instr=yes` profile instruction by instruction.
fn dump_addr(name: &str, addr: *const u8, len: usize) {
    emit(&format!(
        "--- {name} entry at {addr:p} len {len} ({:#x}..{:#x}) ---\n",
        addr as usize,
        addr as usize + len
    ));
    let Some(dir) = std::env::var("MUTSU_JIT_DUMP_BIN").ok() else {
        return;
    };
    // SAFETY: `addr`/`len` describe the finalized, immutable code buffer the
    // module just published; it lives for the process lifetime.
    let bytes = unsafe { std::slice::from_raw_parts(addr, len) };
    let _ = std::fs::create_dir_all(&dir);
    let _ = std::fs::write(
        std::path::Path::new(&dir).join(format!("{name}.bin")),
        bytes,
    );
}

// ---- hooks called from the compiler around `Module::define_function` -------

/// Before codegen: the opcode range and the CLIF, and arm the backend's own
/// disassembler when `asm` was asked for.
pub(super) fn before_define(
    mode: DumpMode,
    name: &str,
    code: &CompiledCode,
    start: usize,
    end: usize,
    ctx: &mut cranelift_codegen::Context,
) {
    if !mode.on() {
        return;
    }
    dump_ops(name, code, start, end);
    dump_clif(name, &ctx.func);
    ctx.set_disasm(mode.asm());
}

/// After codegen, before the context is cleared: the disassembly and the
/// per-opcode byte spans. Returns the emitted code length, which
/// [`after_finalize`] needs to bound the code image.
pub(super) fn after_define(mode: DumpMode, name: &str, ctx: &cranelift_codegen::Context) -> usize {
    if !mode.on() {
        return 0;
    }
    let Some(cc) = ctx.compiled_code() else {
        return 0;
    };
    let spans: Vec<(u32, u32, u32)> = cc
        .buffer
        .get_srclocs_sorted()
        .iter()
        .map(|l| (l.start, l.end, l.loc.bits()))
        .collect();
    let len = cc.code_buffer().len();
    dump_code(name, cc.vcode.as_deref(), len, &spans);
    len
}

/// After the module publishes the code: the entry address, and the code image
/// itself when `MUTSU_JIT_DUMP_BIN` asked for it.
pub(super) fn after_finalize(mode: DumpMode, name: &str, addr: *const u8, len: usize) {
    if mode.on() {
        dump_addr(name, addr, len);
    }
}
