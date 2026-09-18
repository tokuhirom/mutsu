//! The compilation unit a bytecode chunk belongs to (ADR-0106 Slice 0).
//!
//! [`crate::opcode::CompiledCode`] carries a static ip -> line table
//! (`op_lines`), so it can say "instruction 412 is line 89" but not "…of which
//! file". The file was only recoverable at runtime, from three unrelated
//! places: a `?FILE` env probe (`Interpreter::current_source_file`), the
//! per-frame `RoutineFrame { file, def_file }`, and `CompiledFunction::
//! source_file`. None of them is addressable from `(chunk, ip)` alone, which is
//! what a sampler — or a backtrace, or a `--dump-bytecode` annotation — needs.
//!
//! The file is compile-time data, so it is stamped onto every chunk as it is
//! built. Rather than thread a path through the ~40 `Compiler` construction
//! sites (chunk compilers for sub bodies, method bodies, declaration-expression
//! chunks, …), the unit's identity is published for the duration of a compile
//! through this thread-local and read once by `CompiledCode::new()`. That is
//! the same shape the parser already uses for `$?FILE`
//! (`crate::parser::set_parser_source_file`), and it covers *every* chunk a
//! compile produces, including the nested ones no walker enumerates.
//!
//! Compiles that learn their file only afterwards (a nested named sub, stamped
//! from its enclosing routine's definition) are served by
//! `CompiledCode::stamp_source_file` instead.

use crate::symbol::Symbol;
use std::cell::Cell;

thread_local! {
    /// The compilation unit currently being compiled on this thread, or `None`
    /// outside any compile (a hand-built chunk, a `#[test]` helper).
    static UNIT_SOURCE_FILE: Cell<Option<Symbol>> = const { Cell::new(None) };
}

/// The file every chunk built on this thread right now belongs to.
#[inline]
pub(crate) fn current() -> Option<Symbol> {
    UNIT_SOURCE_FILE.with(|cell| cell.get())
}

/// Publishes a compilation unit's file for the lifetime of the guard, restoring
/// the previous one on drop (compiles nest: a module load runs inside the
/// script's own `run()`, and an `EVAL` inside whatever is executing).
pub(crate) struct UnitSourceFileGuard(Option<Symbol>);

impl UnitSourceFileGuard {
    /// Enter a unit whose file is `file`. A `None` leaves the enclosing unit's
    /// file in place rather than clearing it: a caller that cannot name its
    /// file (no `?FILE` in scope) is compiling on behalf of whatever is already
    /// running, and inheriting is a better answer than `None`.
    pub(crate) fn enter(file: Option<Symbol>) -> Self {
        let previous = UNIT_SOURCE_FILE.with(|cell| {
            let previous = cell.get();
            if file.is_some() {
                cell.set(file);
            }
            previous
        });
        Self(previous)
    }
}

impl Drop for UnitSourceFileGuard {
    fn drop(&mut self) {
        let previous = self.0;
        UNIT_SOURCE_FILE.with(|cell| cell.set(previous));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guard_publishes_and_restores() {
        assert_eq!(current(), None);
        let outer = Symbol::intern("outer.raku");
        {
            let _g = UnitSourceFileGuard::enter(Some(outer));
            assert_eq!(current(), Some(outer));
            let inner = Symbol::intern("inner.rakumod");
            {
                let _g = UnitSourceFileGuard::enter(Some(inner));
                assert_eq!(current(), Some(inner));
            }
            assert_eq!(current(), Some(outer));
        }
        assert_eq!(current(), None);
    }

    #[test]
    fn a_none_inherits_rather_than_clears() {
        let outer = Symbol::intern("inherit.raku");
        let _g = UnitSourceFileGuard::enter(Some(outer));
        {
            let _inner = UnitSourceFileGuard::enter(None);
            assert_eq!(current(), Some(outer));
        }
        assert_eq!(current(), Some(outer));
    }

    /// Every chunk a compile produces — the unit's own, its nested closure
    /// bodies, and the bodies of the routines it declares — carries the unit's
    /// file, and `location_at` agrees with `line_at` on every ip.
    #[test]
    fn a_guarded_compile_stamps_every_chunk_it_produces() {
        let file = Symbol::intern("stamped.raku");
        let src = r#"
            sub outer($n) {
                my $add = -> $x { $x + $n };
                sub inner($m) { $m * 2 }
                $add(inner($n));
            }
            outer(3);
        "#;
        let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("parse");
        let (code, fns) = {
            let _g = UnitSourceFileGuard::enter(Some(file));
            let mut compiler = crate::compiler::Compiler::new();
            compiler.is_mainline = true;
            compiler.compile(&stmts)
        };

        let mut chunks = Vec::new();
        collect_chunks(&code, &mut chunks);
        for function in fns.values() {
            collect_chunks(&function.code, &mut chunks);
        }
        assert!(
            chunks.len() > 1,
            "fixture should produce nested chunks, got {}",
            chunks.len()
        );
        for chunk in &chunks {
            assert_eq!(
                chunk.source_file,
                Some(file),
                "a chunk compiled inside the guard did not name its unit"
            );
            for ip in 0..chunk.ops.len() {
                assert_eq!(
                    chunk.location_at(ip).map(|(_, line)| i64::from(line)),
                    chunk.line_at(ip),
                    "location_at disagreed with line_at at ip {ip}"
                );
                if let Some((chunk_file, _)) = chunk.location_at(ip) {
                    assert_eq!(chunk_file, file);
                }
            }
        }
    }

    /// The acceptance fixture: a script that `use`s a module and runs an
    /// `EVAL`. Every chunk reachable afterwards names a file, and it is the
    /// file its code was actually written in — the module for the module's
    /// routine, the script for the script's, and the `EVAL`'s own unit name
    /// (`EVAL_<N>`, what a backtrace and `Code.file` report) for the closure
    /// the snippet produced.
    #[test]
    fn a_script_a_module_and_an_eval_each_name_their_own_unit() {
        let dir =
            std::env::temp_dir().join(format!("mutsu-unit-source-file-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create scratch dir");
        let module_path = dir.join("UnitFileFixture.rakumod");
        let script_path = dir.join("unit-file-fixture.raku");
        std::fs::write(
            &module_path,
            "unit module UnitFileFixture;\nour sub from-module() is export { 41 }\n",
        )
        .expect("write module");
        let script = format!(
            "use lib '{lib}';\n\
             use UnitFileFixture;\n\
             sub from-script() {{ from-module() + 1 }}\n\
             our $from-eval = EVAL 'sub ($x) {{ $x * 2 }}';\n\
             from-script() + $from-eval(1);\n",
            lib = dir.display()
        );
        std::fs::write(&script_path, &script).expect("write script");

        let mut interp = crate::runtime::Interpreter::new();
        interp.set_program_path(&script_path.to_string_lossy());
        interp.run(&script).expect("fixture program should run");

        // The routines the run registered: every chunk of every compiled body
        // names a file, and the two units are told apart.
        let mut seen_module = false;
        let mut seen_script = false;
        for def in interp.registry().functions.values() {
            let Some(compiled) = &def.compiled else {
                continue;
            };
            let mut chunks = Vec::new();
            collect_chunks(&compiled.code, &mut chunks);
            for chunk in chunks {
                let file = chunk.source_file.unwrap_or_else(|| {
                    panic!(
                        "routine {} has a chunk with no file identity",
                        def.name.as_str()
                    )
                });
                seen_module |= file.as_str().contains("UnitFileFixture.rakumod");
                seen_script |= file.as_str().contains("unit-file-fixture.raku");
            }
        }
        assert!(seen_module, "no chunk named the `use`d module");
        assert!(seen_script, "no chunk named the main script");

        // The `EVAL`'d unit, observed through the closure it handed back: its
        // chunk names the EVAL unit, not the script that called `EVAL`.
        let eval_closure = interp
            .env()
            .get("$from-eval")
            .or_else(|| interp.env().get("from-eval"))
            .cloned()
            .expect("the EVAL'd closure should survive the run");
        // A top-level `our $x` holds its value in a container cell, so look
        // through it to reach the Sub itself.
        let eval_closure = match eval_closure.view() {
            crate::value::ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
            _ => eval_closure,
        };
        let crate::value::ValueView::Sub(sub) = eval_closure.view() else {
            panic!("EVAL should have produced a Sub");
        };
        let code = sub
            .compiled_code
            .as_ref()
            .expect("the EVAL'd closure should carry its compiled chunk");
        let file = code
            .source_file
            .expect("the EVAL'd closure's chunk has no file identity");
        assert!(
            file.as_str().starts_with("EVAL_"),
            "an EVAL'd chunk should name its own unit, got {}",
            file.as_str()
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    fn collect_chunks<'a>(
        code: &'a crate::opcode::CompiledCode,
        out: &mut Vec<&'a crate::opcode::CompiledCode>,
    ) {
        out.push(code);
        for nested in &code.closure_compiled_codes {
            collect_chunks(nested, out);
        }
    }
}
