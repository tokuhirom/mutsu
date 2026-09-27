//! The compile-time resolver `$*R` a module's `sub EXPORT` sees, and the
//! phasers it attaches to the importing scope.
//!
//! Under Rakudo's RakuAST frontend a module that wants to run code when the
//! scope that `use`d it is left (FINALIZER's "finalize my resources" idiom)
//! asks the resolver for that scope and hands it a phaser:
//!
//! ```raku
//! ($*R.find-attach-target('block') // $*R.find-attach-target('compunit'))
//!   .add-leave-phaser: LeavePhaser.new(&FINALIZE)
//! ```
//!
//! mutsu's compile-time surface is the RakuAST-shaped one (ADR-0098 §2.1 —
//! there is no `$*W` World), so this is the branch it honours. mutsu runs a
//! module's `EXPORT` when the `use` statement *executes* rather than when it is
//! compiled, but it executes it at exactly the right place: inside the
//! importing block's [`crate::opcode::OpCode::ImportScope`] region, once per
//! entry into that block. So "attach a LEAVE phaser to the block" becomes
//! "queue this callable on the block's open import scope", and the region runs
//! the queue, last attached first, however the block is left. That is the same
//! observable behaviour as a LEAVE compiled into the block, with one ordering
//! caveat: an attached phaser runs after the LEAVE phasers written in the
//! block itself, as though it had been declared before all of them.
//!
//! The attach targets mirror what rakudo reports (measured on 2026.07 with
//! `RAKUDO_RAKUAST=1`): `'block'` is the innermost enclosing block or routine
//! body and is `Nil` at a compunit's top level, `'compunit'` is always the
//! compunit that holds the `use`, and any other name is `Nil`.
//!
//! The BEGIN-time preload (`OpCode::PreloadModule`) also runs `EXPORT`, but at
//! the head of the unit rather than at the `use`; the in-position `use` runs it
//! again where it belongs. A phaser attached during the preload is therefore
//! dropped, or the block would get it twice.

use super::*;
use crate::value::ValueView;

/// `$*R.^name` under rakudo's RakuAST frontend.
pub(crate) const RESOLVER_CLASS: &str = "RakuAST::Resolver::Compile";
/// An attach target handle. It stands for the block / compunit node rakudo
/// would hand back, and only has to accept `add-leave-phaser`.
pub(crate) const ATTACH_TARGET_CLASS: &str = "Mutsu::AttachTarget";

/// The LEAVE phasers attached to one module body's own compunit while it
/// loads, plus the import-scope depth its body started at: a `use` whose
/// depth is no deeper than that is at the module's top level, with no
/// enclosing block of its own.
pub(crate) struct CompunitLeaveFrame {
    pub(crate) import_base: usize,
    pub(crate) phasers: Vec<Value>,
}

/// Where an attach target handle files a phaser.
enum Target {
    /// The import scope at this 1-based depth of `import_scope_stack`.
    Block(usize),
    /// The compunit frame at this depth: 0 is the main program, `n` is
    /// `compunit_leave_frames[n - 1]`.
    Compunit(usize),
    /// Attached during the BEGIN-time preload; the in-position `use` attaches
    /// again.
    Discard,
}

fn int_attr(target: &Value, name: &str) -> Option<i64> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    match attributes.as_map().get(name)?.view() {
        ValueView::Int(n) => Some(n),
        _ => None,
    }
}

fn target_handle(kind: &str, depth: usize) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("kind".to_string(), Value::str(kind.to_string()));
    attrs.insert("depth".to_string(), Value::int(depth as i64));
    Value::make_instance(crate::symbol::Symbol::intern(ATTACH_TARGET_CLASS), attrs)
}

impl Interpreter {
    /// Bind `$*R` for the duration of a `sub EXPORT` call, recording the
    /// in-position `use` it answers for (see the module docs). Always a fresh
    /// binding: every `use` has its own attach targets.
    pub(super) fn bind_compile_time_resolver(&mut self) {
        let depth = self.use_attach_depth.map_or(-1, |d| d as i64);
        let mut attrs = HashMap::new();
        attrs.insert("depth".to_string(), Value::int(depth));
        attrs.insert(
            "compunit".to_string(),
            Value::int(self.compunit_leave_frames.len() as i64),
        );
        self.env.insert(
            "*R".to_string(),
            Value::make_instance(crate::symbol::Symbol::intern(RESOLVER_CLASS), attrs),
        );
    }

    /// Run `body` as the in-position `use` at the current import-scope depth,
    /// so a `$*R` bound by its EXPORT resolves against this block.
    pub(crate) fn with_use_attach_depth<T>(&mut self, body: impl FnOnce(&mut Self) -> T) -> T {
        let saved = self.use_attach_depth.replace(self.import_scope_stack.len());
        let result = body(self);
        self.use_attach_depth = saved;
        result
    }

    /// Run `body` with no in-position `use` recorded: a BEGIN-time preload.
    pub(crate) fn without_use_attach_depth<T>(&mut self, body: impl FnOnce(&mut Self) -> T) -> T {
        let saved = self.use_attach_depth.take();
        let result = body(self);
        self.use_attach_depth = saved;
        result
    }

    /// Run a module body — or an `EVAL`'d string, which is a compunit too — as
    /// its own compunit, then its attached LEAVE phasers. A body that died
    /// keeps its own error; a phaser error only surfaces when the body
    /// succeeded.
    pub(crate) fn run_compunit<T>(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        self.compunit_leave_frames.push(CompunitLeaveFrame {
            import_base: self.import_scope_stack.len(),
            phasers: Vec::new(),
        });
        let result = body(self);
        let frame = self
            .compunit_leave_frames
            .pop()
            .expect("compunit leave frame pushed above");
        let leave = self.run_attached_leave_phasers(frame.phasers);
        let value = result?;
        leave.map(|()| value)
    }

    /// The main program's attached LEAVE phasers, run once its mainline is done.
    pub(crate) fn run_mainline_leave_phasers(&mut self) -> Result<(), RuntimeError> {
        let phasers = std::mem::take(&mut self.mainline_leave_phasers);
        self.run_attached_leave_phasers(phasers)
    }

    /// Take the LEAVE phasers attached to the innermost open import scope.
    pub(crate) fn take_import_scope_leave_phasers(&mut self) -> Vec<Value> {
        self.import_scope_stack
            .last_mut()
            .map(|scope| std::mem::take(&mut scope.leave_phasers))
            .unwrap_or_default()
    }

    /// Run attached LEAVE phasers last-attached first. Every phaser runs even
    /// when an earlier one dies; the first error is the one reported.
    // Cost: O(p) calls, p = attached phasers.
    pub(crate) fn run_attached_leave_phasers(
        &mut self,
        phasers: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let mut first_err = None;
        for code in phasers.into_iter().rev() {
            if let Err(e) = self.call_sub_value(code, Vec::new(), false)
                && first_err.is_none()
            {
                first_err = Some(e);
            }
        }
        first_err.map_or(Ok(()), Err)
    }

    /// Native methods of `$*R` and its attach targets. `None` for a method
    /// neither has, so ordinary dispatch reports it.
    pub(crate) fn dispatch_attach_target_method(
        &mut self,
        class_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match (class_name, method) {
            // Cost: O(1).
            (RESOLVER_CLASS, "find-attach-target") => {
                let name = args.first().map(Value::to_string_value).unwrap_or_default();
                Some(Ok(self.find_attach_target(target, &name)))
            }
            // Cost: O(1) plus the phaser's `meta-object` call.
            (ATTACH_TARGET_CLASS, "add-leave-phaser") => {
                let Some(phaser) = args.first().cloned() else {
                    return Some(Err(RuntimeError::new(
                        "add-leave-phaser needs the phaser to attach",
                    )));
                };
                Some(self.add_leave_phaser(target, phaser))
            }
            _ => None,
        }
    }

    fn find_attach_target(&self, resolver: &Value, name: &str) -> Value {
        let depth = int_attr(resolver, "depth").unwrap_or(-1);
        let compunit = int_attr(resolver, "compunit").unwrap_or(0).max(0) as usize;
        if depth < 0 {
            // The preload's EXPORT run: see the module docs.
            return match name {
                "block" | "compunit" => target_handle("discard", 0),
                _ => Value::NIL,
            };
        }
        let depth = depth as usize;
        match name {
            "block" => {
                let base = compunit
                    .checked_sub(1)
                    .and_then(|i| self.compunit_leave_frames.get(i))
                    .map_or(0, |frame| frame.import_base);
                if depth > base {
                    target_handle("block", depth)
                } else {
                    Value::NIL
                }
            }
            "compunit" => target_handle("compunit", compunit),
            _ => Value::NIL,
        }
    }

    fn add_leave_phaser(&mut self, target: &Value, phaser: Value) -> Result<Value, RuntimeError> {
        let kind = match target.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("kind")
                .map(Value::to_string_value)
                .unwrap_or_default(),
            _ => String::new(),
        };
        let depth = int_attr(target, "depth").unwrap_or(0).max(0) as usize;
        let place = match kind.as_str() {
            "block" => Target::Block(depth),
            "compunit" => Target::Compunit(depth),
            _ => Target::Discard,
        };
        if matches!(place, Target::Discard) {
            return Ok(Value::NIL);
        }
        // The phaser node's `meta-object` is the code it runs — for a phaser
        // written in source that is its compiled block; a subclass that wraps
        // an already-compiled callable (FINALIZER's `LeavePhaser`) returns that.
        // Read through the attribute container a `method meta-object { &!code }`
        // hands back: the queue stores the callable itself.
        let code = self
            .call_method_with_values(phaser, "meta-object", Vec::new())?
            .deref_container();
        match place {
            Target::Block(depth) => match depth
                .checked_sub(1)
                .and_then(|i| self.import_scope_stack.get_mut(i))
            {
                Some(scope) => scope.leave_phasers.push(code),
                None => {
                    return Err(RuntimeError::new(
                        "add-leave-phaser: the block this target names has already been left",
                    ));
                }
            },
            Target::Compunit(0) => self.mainline_leave_phasers.push(code),
            Target::Compunit(n) => match self.compunit_leave_frames.get_mut(n - 1) {
                Some(frame) => frame.phasers.push(code),
                None => {
                    return Err(RuntimeError::new(
                        "add-leave-phaser: the compunit this target names has finished loading",
                    ));
                }
            },
            Target::Discard => {}
        }
        Ok(Value::NIL)
    }

    /// Every attached-but-not-yet-run LEAVE phaser, for the GC root walk.
    pub(crate) fn attached_leave_phasers(&self) -> impl Iterator<Item = &Value> {
        self.import_scope_stack
            .iter()
            .flat_map(|scope| scope.leave_phasers.iter())
            .chain(
                self.compunit_leave_frames
                    .iter()
                    .flat_map(|frame| frame.phasers.iter()),
            )
            .chain(self.mainline_leave_phasers.iter())
    }
}
