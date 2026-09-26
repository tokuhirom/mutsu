//! Choosing how `OpCode::SetVarDynamic` prepares a `my` binding before its
//! initializer runs ([`DeclReset`]).
//!
//! An ordinary `my` in a loop body reuses the binding its previous iteration
//! left behind, and Raku still shows a fresh `Any` there while the initializer
//! runs and after it fails. So the VM resets that binding first
//! (`DeclReset::Fresh`) -- an env write per declaration per iteration, the
//! +7%/+12.5% instruction step #9537 measured on `benchmarks/time-parts.raku`.
//!
//! The reset is only observable by code that can read the name while the
//! initializer runs or after it aborted: a closure, routine or `EVAL` in the
//! body, or the body's own CATCH/CONTROL handlers and phasers. After a loop
//! body is compiled, [`Compiler::relax_loop_body_decl_resets`] proves there is
//! none of those and downgrades the body's declarations to
//! `DeclReset::SeedIfUnbound`: the source body has no CATCH, CONTROL or phaser,
//! no code object was compiled for it (so nothing in it can close over its
//! lexicals), and every opcode emitted for it is on an allowlist of ops that
//! cannot reach code declared in the body. The check fails closed: any
//! unlisted opcode keeps `Fresh`, so a missed case only costs speed.

use super::Compiler;
use crate::ast::Stmt;
use crate::opcode::{DeclReset, OpCode};
use std::cell::Cell;

thread_local! {
    /// How many `Compiler`s this thread has created. Every code object -- a
    /// closure, a routine, a method, a phaser body compiled on its own -- is
    /// compiled by a fresh `Compiler`, so an unchanged count across a loop
    /// body's compilation proves the body holds none.
    static COMPILERS_CREATED: Cell<u64> = const { Cell::new(0) };
}

/// Called by `Compiler::new`.
pub(super) fn note_compiler_created() {
    COMPILERS_CREATED.with(|c| c.set(c.get() + 1));
}

/// Where a loop body's compilation started: its first opcode and the
/// `Compiler` count at that point.
pub(super) struct LoopBodyMark {
    start: usize,
    compilers: u64,
}

impl Compiler {
    pub(super) fn loop_body_decl_reset_mark(&self) -> LoopBodyMark {
        LoopBodyMark {
            start: self.code.ops.len(),
            compilers: COMPILERS_CREATED.with(Cell::get),
        }
    }

    /// Downgrade the `DeclReset::Fresh` declarations of the loop body compiled
    /// since `mark` to `DeclReset::SeedIfUnbound` when nothing can observe the
    /// skipped reset (see the module docs). `source_body` is the body as
    /// written, before `expand_loop_phasers` lowered it to `lowered`.
    // Cost: O(n), n = opcodes emitted for the body; once per compiled loop.
    pub(super) fn relax_loop_body_decl_resets(
        &mut self,
        mark: LoopBodyMark,
        source_body: &[Stmt],
        lowered: &[Stmt],
    ) {
        let observable = source_body.len() != lowered.len()
            || source_body
                .iter()
                .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_) | Stmt::Phaser { .. }))
            || Self::stmts_have_enter_phaser_expr(source_body)
            || COMPILERS_CREATED.with(Cell::get) != mark.compilers;
        if observable {
            return;
        }
        let body = &mut self.code.ops[mark.start..];
        if !body.iter().all(op_cannot_reach_body_code) {
            return;
        }
        for op in body {
            if let OpCode::SetVarDynamic {
                dynamic: false,
                reset: reset @ DeclReset::Fresh,
                ..
            } = op
            {
                *reset = DeclReset::SeedIfUnbound;
            }
        }
    }
}

/// Whether `op` runs no code that a loop body could have declared: constants,
/// frame-local reads and writes, arithmetic and comparisons, and jumps. (An
/// operand's coercion may still call a method, but a class declared in the
/// body would have compiled its methods with a fresh `Compiler`.)
fn op_cannot_reach_body_code(op: &OpCode) -> bool {
    matches!(
        op,
        OpCode::SetVarDynamic { .. }
            | OpCode::LoadConst(_)
            | OpCode::LoadNil
            | OpCode::LoadTrue
            | OpCode::LoadFalse
            | OpCode::GetLocal(_)
            | OpCode::GetLocalMetaAssign { .. }
            | OpCode::GetGlobal(_)
            | OpCode::SetLocal(_)
            | OpCode::SetLocalDecl { .. }
            | OpCode::CheckReadOnly(_)
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Mod
            | OpCode::IntDiv
            | OpCode::IntMod
            | OpCode::Negate
            | OpCode::NumEq
            | OpCode::NumNe
            | OpCode::NumLt
            | OpCode::NumLe
            | OpCode::NumGt
            | OpCode::NumGe
            | OpCode::Jump(_)
            | OpCode::JumpIfFalse(_)
            | OpCode::JumpIfTrue(_)
            | OpCode::Dup
            | OpCode::Pop
    )
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::opcode::{DeclReset, OpCode};

    /// The `DeclReset` of every `SetVarDynamic` in the unit's mainline.
    fn resets(src: &str) -> Vec<DeclReset> {
        let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);
        code.ops
            .iter()
            .filter_map(|op| match op {
                OpCode::SetVarDynamic { reset, .. } => Some(*reset),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn arithmetic_loop_body_skips_the_reset() {
        let r =
            resets("my $t = 0; for ^3 -> $i { my $a = $i * 2; my $b = $a mod 3; $t += $a + $b }");
        assert_eq!(
            r,
            [
                DeclReset::Fresh,
                DeclReset::SeedIfUnbound,
                DeclReset::SeedIfUnbound
            ]
        );
    }

    #[test]
    fn catch_or_phaser_in_the_body_keeps_the_reset() {
        for src in [
            "for ^3 -> $i { my $a = $i * 2; CATCH { default { } } }",
            "for ^3 -> $i { my $a = $i * 2; LEAVE { } }",
            "for ^3 -> $i { my $a = $i * 2; NEXT { } }",
            "my $n = 0; while $n++ < 3 { my $a = $n * 2; CATCH { default { } } }",
        ] {
            assert!(resets(src).iter().all(|r| *r == DeclReset::Fresh), "{src}");
        }
    }

    #[test]
    fn code_in_the_body_keeps_the_reset() {
        for src in [
            // A closure in the initializer can read the declaration.
            "for ^3 { my $y = ({ $y // 0 })() + 1 }",
            // So can a routine declared in the body.
            "for ^3 { my $y = g() + 1; sub g { $y // 0 } }",
            // Any call may reach EVAL.
            "for ^3 { my $y = EVAL('1') }",
        ] {
            assert!(resets(src).iter().all(|r| *r == DeclReset::Fresh), "{src}");
        }
    }

    #[test]
    fn dynamic_declaration_keeps_the_reset() {
        assert!(
            resets("for ^3 -> $i { my $*d = $i + 1 }")
                .iter()
                .all(|r| *r == DeclReset::Fresh)
        );
    }
}
