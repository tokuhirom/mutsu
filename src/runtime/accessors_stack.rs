//! Routine / block / gather execution-stack accessors and current-package state.
use super::*;

impl Interpreter {
    pub(crate) fn routine_stack_top(&self) -> Option<&super::RoutineFrame> {
        self.routine_stack.last()
    }

    pub(crate) fn routine_stack(&self) -> &[super::RoutineFrame] {
        &self.routine_stack
    }

    /// Push a new routine frame. `line` and `file` record the call-site
    /// in the *caller* (the line/file where this function was called from);
    /// `def_file` is the file the routine's body lives in (None = main
    /// script), used by backtrace rendering.
    pub(crate) fn push_routine_with_location(
        &mut self,
        package: String,
        name: String,
        line: Option<u32>,
        file: Option<String>,
        def_file: Option<String>,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            name,
            line,
            file,
            is_method: false,
            is_block: false,
            def_file,
        });
    }

    pub(crate) fn push_method_routine_with_location(
        &mut self,
        package: String,
        name: String,
        line: Option<u32>,
        file: Option<String>,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            name,
            line,
            file,
            is_method: true,
            is_block: false,
            def_file: None,
        });
    }

    /// Push a block/closure routine frame.
    pub(crate) fn push_block_routine_with_location(
        &mut self,
        package: String,
        name: String,
        line: Option<u32>,
        file: Option<String>,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            name,
            line,
            file,
            is_method: false,
            is_block: true,
            def_file: None,
        });
    }

    pub(crate) fn pop_routine(&mut self) {
        self.routine_stack.pop();
    }

    /// Current routine-stack depth. Paired with [`truncate_routine_stack`] so a
    /// structured execution boundary (block scope, try/catch) can record its
    /// entry depth and restore it on exit, exception-safely.
    pub(crate) fn routine_stack_len(&self) -> usize {
        self.routine_stack.len()
    }

    /// Drop routine frames down to `len`. Used by block/try executors to remove
    /// the bare-block callframe they pushed (and reclaim any frames a nested
    /// bare block leaked when its body threw past its own cleanup).
    pub(crate) fn truncate_routine_stack(&mut self, len: usize) {
        self.routine_stack.truncate(len);
    }

    pub(crate) fn block_stack_top(&self) -> Option<&Value> {
        self.block_stack.last()
    }

    pub(crate) fn push_block(&mut self, val: Value) {
        self.block_stack.push(val);
    }

    pub(crate) fn pop_block(&mut self) {
        self.block_stack.pop();
    }

    /// Stringify a value, calling the `.Str` method for Instance and Package types.
    pub(crate) fn stringify_value(&mut self, value: Value) -> Result<String, RuntimeError> {
        match value.view() {
            ValueView::Instance { .. } | ValueView::Package(_) => {
                let result = self.call_method_with_values(value, "Str", vec![])?;
                Ok(result.to_string_value())
            }
            _ => Ok(value.to_string_value()),
        }
    }

    /// Check if a value can respond to a given method name.
    pub(crate) fn value_can_method(&mut self, value: &Value, method: &str) -> bool {
        // Check builtin 0-arg method (covers most built-in methods)
        if crate::builtins::native_method_0arg(value, crate::symbol::Symbol::intern(method))
            .is_some()
        {
            return true;
        }
        // For instances, check class methods
        if let ValueView::Instance { class_name, .. } = value.view()
            && self.class_has_method(&class_name.resolve(), method)
        {
            return true;
        }
        // For type objects (`Chemistry::Elements.^can(...)` / `can-ok $type,
        // ...`), resolve methods against the named class's MRO too — a type
        // object can do any of its class's methods, not just the universal set.
        if let ValueView::Package(class_name) = value.view()
            && self.class_has_method(&class_name.resolve(), method)
        {
            return true;
        }
        // Universal methods available on all values
        matches!(
            method,
            "WHAT"
                | "say"
                | "print"
                | "put"
                | "gist"
                | "Str"
                | "Int"
                | "Num"
                | "Bool"
                | "Numeric"
                | "Real"
                | "so"
                | "not"
                | "defined"
                | "isa"
                | "can"
                | "does"
                | "ACCEPTS"
                | "raku"
                | "perl"
                | "clone"
                | "new"
        )
    }

    pub(crate) fn take_value(&mut self, val: Value) -> Result<(), RuntimeError> {
        if let Some(items) = self.gather_items.last_mut() {
            // `take` of a Slip flattens it into the gather (`take Empty` /
            // `take slip(1,2)` add zero / two elements — Rakudo semantics);
            // every other value, including a List/Seq, is added as one element
            // (a later `flat`/`.flat` on the gather result flattens those).
            if let ValueView::Slip(elems) = val.view() {
                items.extend(elems.iter().cloned());
            } else {
                items.push(val);
            }
            if let Some(Some(limit)) = self.gather_take_limits.last()
                && items.len() >= *limit
            {
                if self.lazy_take_boundary_defer {
                    // Inside a condition-driven loop: defer the suspension to
                    // the loop's iteration boundary (`gather_suspend_pending`)
                    // — suspending at the take itself replays the statements
                    // between the take and the iteration end on resume. The
                    // overshoot backstop still signals here if no boundary is
                    // ever reached.
                    self.gather_suspend_pending = true;
                    if items.len() >= limit.saturating_add(64) {
                        self.gather_suspend_pending = false;
                        return Err(RuntimeError::new(
                            "__mutsu_lazy_gather_take_limit_reached__",
                        ));
                    }
                } else {
                    return Err(RuntimeError::new(
                        "__mutsu_lazy_gather_take_limit_reached__",
                    ));
                }
            }
        }
        Ok(())
    }

    pub(crate) fn gather_items_len(&self) -> usize {
        self.gather_items.len()
    }

    pub(crate) fn push_gather_items(&mut self, items: Vec<Value>) {
        self.gather_items.push(items);
    }

    pub(crate) fn pop_gather_items(&mut self) -> Option<Vec<Value>> {
        self.gather_items.pop()
    }

    pub(crate) fn push_gather_take_limit(&mut self, limit: Option<usize>) {
        self.gather_take_limits.push(limit);
    }

    pub(crate) fn pop_gather_take_limit(&mut self) {
        self.gather_take_limits.pop();
    }

    /// The package currently in scope, read out of the shared `Arc<RwLock>`
    /// handle as an owned `String`. Returns owned (not `&str`) because the value
    /// lives behind a lock guard that must not escape the call — the guard is
    /// dropped before returning, so no lock is held across the caller's work
    /// (re-entry safe, mirroring the registry accessors).
    pub(crate) fn current_package(&self) -> String {
        self.current_package.read().unwrap().clone()
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        *self.current_package.write().unwrap() = pkg;
    }

    /// Interior-mutable variant for the `&self` regex matcher: the package is
    /// stored behind a RwLock, so a temporary switch (e.g. into a cross-package
    /// grammar subrule's defining package while parsing its body) does not need
    /// `&mut self`.
    pub(crate) fn set_current_package_shared(&self, pkg: String) {
        *self.current_package.write().unwrap() = pkg;
    }
}
