//! Cranelift's IR verifier: when it runs, and what a failure does (ADR-19368).
//!
//! Cranelift documents `enable_verifier` as a development aid ("This makes
//! compilation slower but catches many bugs. The verifier is always enabled
//! by default, which is useful during development."). mutsu runs it in debug
//! builds (`cargo test`, the gate) and wherever `MUTSU_JIT_VERIFY=1` asks for
//! it (the nightly `jit-stress` job), and not in release builds, where it was
//! ~42% of Cranelift's compile time.
//!
//! When it runs, a failure is loud: the IR mutsu generated is invalid, which
//! is a mutsu codegen bug, so the run panics with Cranelift's report and the
//! function's IR. Before ADR-19368 every compiler swallowed the error with
//! `.ok()?` and the region silently ran on the interpreter.

use cranelift_codegen::CodegenError;
use cranelift_module::{FuncId, Module, ModuleError};
use std::sync::OnceLock;

/// Whether the verifier runs: `MUTSU_JIT_VERIFY=1`/`0` when set, otherwise
/// on exactly in debug builds.
// Cost: O(1) — the environment is read once per process.
pub(super) fn verifier_enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| {
        verify_setting(
            std::env::var("MUTSU_JIT_VERIFY").ok().as_deref(),
            cfg!(debug_assertions),
        )
    })
}

/// The verifier setting for a `MUTSU_JIT_VERIFY` value and build kind: `1`
/// or `on` forces it on, `0` or `off` forces it off, anything else (or
/// nothing) follows the build.
fn verify_setting(env: Option<&str>, debug_build: bool) -> bool {
    match env.map(str::trim) {
        Some("1" | "on") => true,
        Some("0" | "off") => false,
        _ => debug_build,
    }
}

/// `module.define_function(id, ctx)`, where a verifier failure panics with
/// the IR instead of declining (ADR-19368 D3). Every other error declines as
/// before (`None`), and the region runs on the interpreter. `what` names the
/// compiler in the panic message.
// Cost: O(n) Cranelift compile, n = the function's instructions.
pub(super) fn define_function_checked(
    module: &mut cranelift_jit::JITModule,
    id: FuncId,
    ctx: &mut cranelift_codegen::Context,
    what: &str,
) -> Option<()> {
    match module.define_function(id, ctx) {
        Ok(()) => Some(()),
        Err(ModuleError::Compilation(CodegenError::Verifier(errors))) => {
            let report =
                cranelift_codegen::print_errors::pretty_verifier_error(&ctx.func, None, errors);
            panic!(
                "mutsu {what}: Cranelift's IR verifier rejected a generated function \
                 (a mutsu codegen bug, ADR-19368):\n{report}"
            );
        }
        Err(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::verify_setting;

    #[test]
    fn the_build_decides_unless_the_environment_does() {
        assert!(verify_setting(None, true));
        assert!(!verify_setting(None, false));
        assert!(verify_setting(Some("1"), false));
        assert!(verify_setting(Some("on"), false));
        assert!(!verify_setting(Some("0"), true));
        assert!(!verify_setting(Some("off"), true));
        assert!(verify_setting(Some("bogus"), true));
        assert!(!verify_setting(Some(""), false));
    }
}
