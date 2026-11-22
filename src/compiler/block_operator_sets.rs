//! The synthetic lexical import set of a block that declares a plain built-in
//! operator routine of its own (#17771; `runtime::block_operator_sets`).

use super::*;
use crate::runtime::block_operator_sets::{BLOCK_OPS_MARK, block_operator_set_names};

impl Compiler {
    /// The plain built-in operator routines `stmts` (a block body below the
    /// unit's top level) declares directly, sorted: a non-multi, literally
    /// named `my` `sub infix:<op>` of an operator whose every use in a block
    /// can read a lexical set (`crate::lexical_builtin_op::binds_lexically`).
    /// A name the block also declares a `multi` or `proto` of is left out: that
    /// family keeps the registry path.
    // Cost: O(s^2) worst case, s = statements of the block (each declaration
    // checks the block for a family of its name); O(s) without one.
    fn block_plain_operator_names(stmts: &[Stmt]) -> Vec<Symbol> {
        let family = |name: Symbol| {
            stmts.iter().any(|stmt| match stmt {
                Stmt::SubDecl {
                    name: n,
                    multi: true,
                    ..
                } => *n == name,
                Stmt::ProtoDecl { name: n, .. } => *n == name,
                _ => false,
            })
        };
        let mut names: Vec<Symbol> = stmts
            .iter()
            .filter_map(Self::plain_operator_decl_name)
            .filter(|name| !family(*name))
            .collect();
        names.sort_by_key(|name| name.resolve());
        names.dedup();
        names
    }

    /// The name `stmt` declares when it is a plain built-in operator routine
    /// ([`Self::block_plain_operator_names`] before the family check).
    // Cost: O(m), m = bytes of the name.
    fn plain_operator_decl_name(stmt: &Stmt) -> Option<Symbol> {
        match stmt {
            Stmt::SubDecl {
                name,
                name_expr: None,
                multi: false,
                is_export: false,
                scope: crate::ast::SubScope::My,
                ..
            } if name.with_str(|n| {
                n.starts_with("infix:<") && crate::lexical_builtin_op::binds_lexically(n)
            }) =>
            {
                Some(*name)
            }
            _ => None,
        }
    }

    /// The index of the statement of `stmts` (a block body) that opens the
    /// block's import region: its first import statement, or its first plain
    /// built-in operator routine, whose operator set the region holds while
    /// the block runs (#17771). `None` when the block has neither.
    // Cost: O(s), s = statements of the block.
    pub(super) fn import_region_start(stmts: &[Stmt]) -> Option<usize> {
        stmts.iter().position(|stmt| {
            Self::opens_import_scope(stmt) || Self::plain_operator_decl_name(stmt).is_some()
        })
    }

    /// The key of the import region of `stmts` (a block body) that statement
    /// `first` opens: the unit's file, the line of that statement and its
    /// index, so a recompile of the same AST (a `.map` callback) names the same
    /// region.
    // Cost: O(s), s = statements of the block.
    pub(super) fn import_region_key(&self, stmts: &[Stmt], first: usize) -> Symbol {
        let line = stmts[..first]
            .iter()
            .rev()
            .find_map(|stmt| match stmt {
                Stmt::SetLine(n, _) => Some(*n),
                _ => None,
            })
            .or(self.last_source_line)
            .unwrap_or(0);
        let file = crate::unit_source_file::current()
            .map(|file| file.resolve())
            .unwrap_or_default();
        Symbol::intern(&format!("{file}\u{1}{line}\u{1}{first}"))
    }

    /// Open the operator set of `stmts` (a block body below the unit's top
    /// level) for the code compiled next, when the block declares a plain
    /// built-in operator routine of its own. The id is the block's import
    /// region key ([`Self::import_region_key`]) plus the operators, so a
    /// recompile of the same AST finds the same set, and the block's region
    /// is where the set is recorded while it runs
    /// (`Interpreter::bind_block_operator`). The caller truncates
    /// `lexical_import_chain` when the block ends.
    // Cost: O(s log s) plus the family check, s = statements of the block.
    pub(super) fn enter_block_operator_set(&mut self, stmts: &[Stmt]) {
        let names = Self::block_plain_operator_names(stmts);
        if names.is_empty() {
            return;
        }
        let Some(first) = Self::import_region_start(stmts) else {
            return;
        };
        let region = self.import_region_key(stmts, first);
        let list: Vec<String> = names.iter().map(|name| name.resolve()).collect();
        let id = Symbol::intern(&format!(
            "{BLOCK_OPS_MARK}{}\u{1}{}",
            region.resolve(),
            list.join(",")
        ));
        if self.lexical_import_chain.contains(&id) {
            return;
        }
        self.lexical_import_chain.push(id);
        // The block's own declaration of the name is what binds the set: it
        // must not hide the set the way a routine declared after a `use`
        // hides that import (`lexical_import_chain_idx`). The mark fits a
        // `u32`, which a closure records it as (`enclosing_set_decl_marks`)
        // for a recompile of its body.
        self.lexical_set_decl_mark.insert(id, u32::MAX as usize);
    }

    /// The operator set of the innermost open block that declares the plain
    /// operator routine `name` ([`Self::enter_block_operator_set`]).
    // Cost: O(c * n), c = open import sets, n = length of an id.
    pub(super) fn block_operator_set_for(&self, name: Symbol) -> Option<Symbol> {
        let wanted = name.resolve();
        self.lexical_import_chain.iter().rev().copied().find(|id| {
            let text = id.resolve();
            block_operator_set_names(&text).is_some_and(|mut names| names.any(|n| n == wanted))
        })
    }
}
