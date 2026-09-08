use super::*;

/// The code object a call frame exposes through `callframe().code`,
/// `&?ROUTINE` / `&?BLOCK` and the backtrace, held on `block_stack` and in
/// `CallFrameEntry::code`.
///
/// A *named* routine call used to build its `Sub` eagerly on entry:
/// `Value::make_sub(.., params.clone(), param_defs.clone(), .., clone_env())`
/// -- a `Gc` allocation, two `Vec` deep clones and, because `clone_env`
/// flattens, a whole-lexical-scope map clone whenever the caller's env was a
/// scoped overlay (every call made from inside another routine). Almost no
/// call ever reads the object: the vendored `Test.rakumod`'s assertion loop
/// paid ~16k instructions per assertion building and dropping two of them
/// (`todo/deep/vendor-real-test-module-flip.md`). The frame now records what the
/// object would be built FROM and materializes on first read, caching the
/// result so repeated reads within the frame see one object.
#[derive(Clone)]
pub(crate) enum CodeFrame {
    /// An already-built code object (a closure/block body, or a routine the
    /// interpreter carrier dispatched).
    Ready(Value),
    /// A named routine's frame, built on demand.
    Lazy(std::sync::Arc<LazyRoutineCode>),
}

/// The ingredients of a named routine's `Sub` value, captured at call entry.
pub(crate) struct LazyRoutineCode {
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
    /// The routine being run: a refcount bump of the table's (or the OTF
    /// cache's) own `Arc`, so entry copies no signature vector. The
    /// `params` / `param_defs` the built `Sub` carries are read off it only
    /// if something materializes the frame.
    pub(crate) cf: std::sync::Arc<crate::opcode::CompiledFunction>,
    /// The caller's env as it was at call entry. An `Arc` bump of the live
    /// env: later writes to that env copy-on-write away from this handle, so
    /// it is the same snapshot the eager `clone_env()` took, minus the
    /// flatten -- which happens in `materialize` if anyone asks.
    pub(crate) env: Env,
    materialized: std::sync::OnceLock<Value>,
}

impl LazyRoutineCode {
    pub(crate) fn new(
        package: Symbol,
        name: Symbol,
        cf: std::sync::Arc<crate::opcode::CompiledFunction>,
        env: Env,
    ) -> Self {
        Self {
            package,
            name,
            cf,
            env,
            materialized: std::sync::OnceLock::new(),
        }
    }

    /// The cached code object, if a read has already built it.
    pub(crate) fn built(&self) -> Option<&Value> {
        self.materialized.get()
    }
}

impl CodeFrame {
    /// The routine's `(package, name)` without materializing: `Nil` for a
    /// `Ready` frame that is not a `Sub` (or a `Mixin` over one).
    pub(crate) fn routine_identity(&self) -> Option<(Symbol, Symbol)> {
        match self {
            CodeFrame::Lazy(l) => Some((l.package, l.name)),
            CodeFrame::Ready(v) => {
                let sd = match v.view() {
                    ValueView::Sub(sd) => Some(sd),
                    ValueView::Mixin(inner, _) => match inner.as_ref().view() {
                        ValueView::Sub(sd) => Some(sd),
                        _ => None,
                    },
                    _ => None,
                };
                sd.map(|sd| (sd.package, sd.name))
            }
        }
    }

    /// True when materializing yields a `Sub` (or a `Mixin` over one).
    pub(crate) fn is_sub(&self) -> bool {
        match self {
            CodeFrame::Lazy(_) => true,
            CodeFrame::Ready(v) => match v.view() {
                ValueView::Sub(_) => true,
                ValueView::Mixin(inner, _) => matches!(inner.as_ref().view(), ValueView::Sub(_)),
                _ => false,
            },
        }
    }

    /// Visit the `Value`s this frame roots for the cycle collector: the
    /// object itself when built, and the captured env's own (overlay) values
    /// for a lazy frame -- the parent tiers are rooted by the frames that own
    /// them.
    pub(crate) fn visit_roots(&self, visitor: &mut dyn crate::gc::RootVisitor) {
        match self {
            CodeFrame::Ready(v) => visitor.visit_value(v),
            CodeFrame::Lazy(l) => {
                if let Some(v) = l.built() {
                    visitor.visit_value(v);
                }
                l.env.visit_values(visitor);
            }
        }
    }
}

impl Interpreter {
    /// The code object for `frame`, building a lazy routine frame's `Sub` on
    /// first use exactly as the eager entry path used to: the routine's
    /// params/param_defs, an empty AST body, the caller env flattened to its
    /// full lexical view (a `callframe().code` must expose the whole scope,
    /// not a scoped overlay), and any role ever composed onto the routine
    /// re-applied.
    pub(crate) fn code_frame_value(&self, frame: &CodeFrame) -> Value {
        match frame {
            CodeFrame::Ready(v) => v.clone(),
            CodeFrame::Lazy(l) => l
                .materialized
                .get_or_init(|| {
                    let sub_val = Value::make_sub(
                        l.package,
                        l.name,
                        l.cf.params.clone(),
                        l.cf.param_defs.clone(),
                        vec![],
                        false,
                        l.env.flattened(),
                    );
                    self.materialize_routine_mixins_shared(
                        sub_val,
                        l.package.as_str(),
                        l.name.as_str(),
                    )
                })
                .clone(),
        }
    }
}
