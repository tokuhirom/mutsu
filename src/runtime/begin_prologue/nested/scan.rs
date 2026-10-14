//! What a piece of code a lifted BEGIN runs reads and calls (ADR-0134,
//! slice 2), as the compiler resolves it ([`super::routines`]).

use crate::ast::Stmt;
use crate::opcode::CompiledCode;
use crate::value::{Value, ValueView};
use std::collections::HashSet;

/// The names a piece of code reads, as the compiler resolves them.
pub(super) struct Scan {
    /// The free variables, read or written.
    pub(super) free: Vec<crate::symbol::Symbol>,
    /// The routines called, qualified or not, or read as `&name`.
    pub(super) callees: HashSet<crate::symbol::Symbol>,
    /// The routines among `callees` read as `&name` rather than only called.
    pub(super) code_reads: HashSet<crate::symbol::Symbol>,
    /// What the code does with each routine it reads as `&name`.
    pub(super) code_uses: super::code_uses::CodeUses,
    /// The code can reach a name dynamically (`EVAL`, `CALLER::`, symbolic
    /// lookup), which neither of the sets above can bound.
    pub(super) reflective: bool,
}

impl Scan {
    pub(super) fn of(stmts: &[Stmt]) -> Scan {
        let (code, fns) = crate::compiler::Compiler::new().compile(stmts);
        let mut free = code.free_var_syms.clone();
        // Compiled on its own, an assignment to a code variable the code does
        // not declare (`&g = { ... }`) is an assignment to a routine name,
        // which names no variable at all.
        for name in super::decls::Mentions::of(stmts).code_var_writes() {
            let sym = crate::symbol::Symbol::intern(&name);
            if !free.contains(&sym) {
                free.push(sym);
            }
        }
        let mut scan = Scan {
            free,
            callees: HashSet::new(),
            code_reads: HashSet::new(),
            code_uses: super::code_uses::CodeUses::of(stmts),
            reflective: false,
        };
        scan.absorb_code(&code);
        scan.absorb(&fns);
        scan
    }

    /// The free variables the scanned code reads or writes.
    pub(super) fn free(&self) -> &[crate::symbol::Symbol] {
        &self.free
    }

    /// Fold in the routines declared inside the scanned code. Their bodies are
    /// compiled on their own, so the code's own ops do not hold their calls.
    fn absorb(&mut self, fns: &crate::opcode::CompiledFns) {
        for function in fns.values() {
            self.absorb_code(&function.code);
            if let Some(nested) = &function.compiled_fns {
                self.absorb(nested);
            }
        }
    }

    /// The routine names one chunk refers to: a call (`f(...)`) and a
    /// code-variable read (`&f`, `&f(...)`, `&f.()`). The compiler lists the
    /// latter among the free variables only when an enclosing scope declares
    /// `&f` as a variable, which a routine is not.
    fn absorb_code(&mut self, code: &CompiledCode) {
        self.reflective |= code.needs_reflective_capture;
        self.absorb_ops(code);
    }

    fn absorb_ops(&mut self, code: &CompiledCode) {
        let const_str = |idx: u32| match code.constants.get(idx as usize).map(Value::view) {
            Some(ValueView::Str(s)) => Some(s),
            _ => None,
        };
        for op in &code.ops {
            let read = CompiledCode::op_code_var_read_const_idx(op);
            let called = CompiledCode::op_callee_name_const_idx(op);
            if let Some(name) = read.and_then(const_str) {
                self.code_reads.insert(crate::symbol::Symbol::intern(&name));
            }
            for name in read.into_iter().chain(called).filter_map(const_str) {
                let name = crate::symbol::Symbol::intern(&name);
                // A call qualified by a pseudo-package (`MY::helper()`) names a
                // routine in a scope the called-name scan does not look at.
                self.reflective |= is_pseudo_qualified(name);
                self.callees.insert(name);
            }
            // `::($name)` finds a routine as readily as a type, which no scan
            // of the names in the code can list.
            self.reflective |= matches!(
                op,
                crate::opcode::OpCode::IndirectTypeLookup
                    | crate::opcode::OpCode::IndirectTypeLookupStore
            );
        }
        for nested in &code.closure_compiled_codes {
            self.absorb_ops(nested);
        }
    }
}

/// Whether a called routine is one of the core routines, which run no code the
/// program wrote and so cannot look a name up in the scope that calls them.
/// Any other routine might: an imported one (`throws-like 'helper()'`) or a
/// unit's own (`sub run-it($code) { EVAL $code }`) evaluates its argument where
/// it was called from.
/// A proto's `{*}` is a call too, which dispatches to the candidates in scope,
/// and so is a re-dispatcher (`callsame`, `nextwith`, ...), which the RakuAST
/// frontend lowers to a call where the parser leaves a bare word.
pub(super) fn is_core_routine(name: crate::symbol::Symbol) -> bool {
    !crate::qualified::is_qualified(name)
        && (crate::runtime::Interpreter::is_builtin_function(name.as_str())
            || matches!(
                name.as_str(),
                crate::ast::ONLYSTAR_DISPATCH
                    | "callsame"
                    | "callwith"
                    | "nextsame"
                    | "nextwith"
                    | "samewith"
                    | "nextcallee"
                    | "lastcall"
            ))
}

/// Whether a called name is qualified by a pseudo-package (`MY::helper`,
/// `OUTER::helper`), which names a routine in a scope the bare-name scan does
/// not look at.
pub(super) fn is_pseudo_qualified(name: crate::symbol::Symbol) -> bool {
    crate::qualified::package_parent(name)
        .and_then(|pkg| crate::qualified::package_ancestors(pkg).last())
        .is_some_and(|head| crate::runtime::Interpreter::is_pseudo_package_name(head.as_str()))
}
