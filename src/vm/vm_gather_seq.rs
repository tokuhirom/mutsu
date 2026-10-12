//! The interpreter half of a plain `gather` as a `Seq`
//! (docs/adr/11678-gather-is-a-deferred-seq-source.md): driving the
//! `SeqSource::Gather` coroutine a prefix at a time, and handing its inner
//! list to the consumers that already stream a `LazyList`.

use super::*;

impl Interpreter {
    /// The first `needed` elements of a gather's inner coroutine list
    /// (fewer when it finishes first), running the body only as far as
    /// that takes. The prefix half of [`crate::value::SeqSource::Gather`]'s
    /// pull.
    // Cost: O(needed) for the copy, plus running the body to its
    // `needed`-th `take`.
    pub(crate) fn pull_gather_prefix(
        &mut self,
        list: &Value,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        match list.view() {
            ValueView::LazyList(ll) => self.force_lazy_list_vm_n(&ll, needed),
            _ => Ok(Vec::new()),
        }
    }

    /// An operand a consumer reads by streaming a `LazyList` (the zip and
    /// cross metaoperators): a plain gather `Seq` is handed over as its inner
    /// coroutine list, which those consumers already pull on demand. The
    /// consumer consumes the Seq, as Rakudo's iterating operators do
    /// ([`crate::value::SeqBody::take_gather_source`]); a cached Seq shares
    /// its list instead. Every other value is returned unchanged.
    // Cost: O(1).
    pub(crate) fn gather_operand(&self, value: Value) -> Value {
        if !value.is_seq_value() {
            return value;
        }
        match value.view() {
            ValueView::Seq(body) => body
                .take_gather_source()
                .or_else(|| body.shared_gather_source())
                .unwrap_or(value),
            _ => value,
        }
    }

    /// A plain gather `Seq` handed to a builtin routine as its inner
    /// coroutine list, without consuming the Seq. Only the methods of a
    /// built-in receiver still read a gather this way (ADR-11678 §8; #13026
    /// retires it). Every other value is returned unchanged.
    // Cost: O(1).
    pub(crate) fn shared_gather_list(&self, value: Value) -> Value {
        match value.view() {
            ValueView::Seq(body) => body.gather_source().unwrap_or(value),
            // A named argument (`C.new(c => gather {...})`) carries it as the
            // Pair's value.
            ValueView::Pair(key, inner) if Self::is_gather_seq(inner) => {
                Value::pair(key.clone(), self.shared_gather_list(inner.clone()))
            }
            _ => value,
        }
    }

    /// The arguments of builtin routine `name` with every plain gather `Seq`
    /// read the way Rakudo's routine reads it ([`builtin_gather_read`]):
    /// a streaming routine steals the coroutine and consumes the Seq, an
    /// iterating one pulls it whole and consumes it, and the rest pull it
    /// whole and leave it cached. Borrows `args` unchanged when no argument
    /// is a gather (the common case).
    // Cost: O(n), n = arguments, plus running each gather the routine reads
    // whole to completion.
    pub(crate) fn builtin_gather_args<'a>(
        &mut self,
        name: &str,
        args: &'a [Value],
    ) -> Result<std::borrow::Cow<'a, [Value]>, RuntimeError> {
        if !Self::has_builtin_seq_read(args) {
            return Ok(std::borrow::Cow::Borrowed(args));
        }
        let read = builtin_gather_read(name);
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            // A Seq an earlier read consumed (`sum(s); sum(s)`) cannot be read
            // again, whatever its source was.
            if !matches!(read, GatherRead::Pass) && Self::is_consumed_seq(arg) {
                return Err(crate::value::seq_consumed_error());
            }
            let arg = match arg.view() {
                // A named argument's gather fills what it is bound to
                // (`C.new(c => gather {...})`): pulled whole, left cached.
                ValueView::Pair(_, inner) if Self::is_gather_seq(inner) => {
                    self.reify_gather_seq(inner)?;
                    arg.clone()
                }
                _ if Self::is_gather_seq(arg) => self.read_gather_arg(arg.clone(), read)?,
                _ => arg.clone(),
            };
            out.push(arg);
        }
        Ok(std::borrow::Cow::Owned(out))
    }

    /// One plain gather argument read as `read` says.
    // Cost: O(1) for a stream or a pass; running the gather to completion
    // otherwise.
    fn read_gather_arg(&mut self, value: Value, read: GatherRead) -> Result<Value, RuntimeError> {
        let ValueView::Seq(body) = value.view() else {
            return Ok(value);
        };
        let body = std::sync::Arc::clone(&body);
        match read {
            GatherRead::Pass => Ok(value),
            GatherRead::Stream => Ok(self.gather_operand(value)),
            GatherRead::Consume => {
                let (items, outcome) = self.take_seq_body(&body)?;
                Ok(match outcome {
                    crate::value::SeqTaken::Taken => Value::seq(items),
                    crate::value::SeqTaken::Served => value,
                })
            }
            GatherRead::Cache => {
                self.reify_seq_body(&body)?;
                Ok(value)
            }
        }
    }

    /// Whether `value` is a plain gather `Seq` still pulling its coroutine.
    // Cost: O(1).
    pub(crate) fn is_gather_seq(value: &Value) -> bool {
        value.is_seq_value()
            && matches!(value.view(), ValueView::Seq(body) if body.is_gather_source())
    }

    /// Whether [`Self::builtin_gather_args`] has anything to do for `args`: a
    /// plain gather, or a Seq an earlier read consumed.
    // Cost: O(n), n = arguments.
    pub(crate) fn has_builtin_seq_read(args: &[Value]) -> bool {
        args.iter()
            .any(|a| Self::is_gather_arg(a) || Self::is_consumed_seq(a))
    }

    /// Whether `value` is a `Seq` whose source an earlier read already took.
    // Cost: O(1).
    fn is_consumed_seq(value: &Value) -> bool {
        value.is_seq_value() && matches!(value.view(), ValueView::Seq(body) if body.is_consumed())
    }

    /// [`Self::is_gather_seq`], or a named argument whose value is one.
    // Cost: O(1).
    pub(crate) fn is_gather_arg(value: &Value) -> bool {
        Self::is_gather_seq(value)
            || matches!(value.view(), ValueView::Pair(_, inner) if Self::is_gather_seq(inner))
    }

    /// [`Self::shared_gather_list`] over a builtin's argument list, borrowing
    /// it unchanged when no argument is a gather (the common case).
    // Cost: O(n), n = arguments.
    pub(crate) fn shared_gather_args<'a>(
        &self,
        args: &'a [Value],
    ) -> std::borrow::Cow<'a, [Value]> {
        if !args.iter().any(Self::is_gather_arg) {
            return std::borrow::Cow::Borrowed(args);
        }
        std::borrow::Cow::Owned(
            args.iter()
                .cloned()
                .map(|a| self.shared_gather_list(a))
                .collect(),
        )
    }

    /// Run a plain gather `Seq` to completion before a reader that renders it
    /// through pure code (`say`, `put`, `print`, `note`): Rakudo's rendering
    /// iterates the whole Seq too. A no-op for every other value.
    // Cost: runs the gather body to completion.
    pub(crate) fn reify_gather_seq(&mut self, value: &Value) -> Result<(), RuntimeError> {
        if let ValueView::Seq(body) = value.view()
            && body.is_gather_source()
        {
            let body = std::sync::Arc::clone(&body);
            self.reify_seq_body(&body)?;
        }
        Ok(())
    }

    /// An `@`/`%` attribute's initializer owns its elements: a deferred
    /// `.map`/`.grep` or a plain gather is run before the pure sigil coercion
    /// reads it (`has @.c = gather {...}`, ADR-11678). A no-op for a `$`
    /// attribute, which keeps the Seq itself.
    // Cost: runs a deferred Seq to completion; O(1) otherwise.
    pub(crate) fn reify_list_attr_value(
        &mut self,
        value: &Value,
        sigil: char,
    ) -> Result<(), RuntimeError> {
        if matches!(sigil, '@' | '%') {
            self.reify_map_grep_seq(value)?;
            self.reify_gather_seq(value)?;
        }
        Ok(())
    }
}

/// How a builtin routine reads a plain gather argument, measured against
/// Rakudo (`t/collections/lazy-seq/gather-builtin-args.t` pins each row).
#[derive(Clone, Copy)]
enum GatherRead {
    /// The routine is handed the Seq itself and reads no element (`list`).
    Pass,
    /// The routine streams its input, so an infinite gather works, and
    /// consumes the Seq (`first`, `zip`, `map`): the coroutine is stolen, the
    /// ADR-11678 §2.3 steal.
    Stream,
    /// The routine iterates the Seq whole and consumes it (`sum`, `max`,
    /// `unique`, `classify`): a later read throws `X::Seq::Consumed`.
    Consume,
    /// The routine reads the elements through the Seq's cache (`elems`,
    /// `join`, `sort`, `set`, every `Str`/`Numeric` coercion): pulled whole,
    /// left readable.
    Cache,
}

/// [`GatherRead`] for builtin routine `name`. Unlisted routines read through
/// the cache, which keeps the Seq readable and gives the pure builtins the
/// elements.
// Cost: O(1).
fn builtin_gather_read(name: &str) -> GatherRead {
    match name {
        "list" | "item" | "defined" | "push" | "unshift" => GatherRead::Pass,
        "first" | "zip" | "roundrobin" | "flat" | "map" | "grep" => GatherRead::Stream,
        "sum" | "min" | "max" | "minmax" | "unique" | "repeated" | "squish" | "pick" | "roll"
        | "classify" | "categorize" | "produce" | "reduce" | "keys" | "values" | "kv" | "pairs"
        | "permutations" | "combinations" | "cross" | "append" | "prepend" => GatherRead::Consume,
        _ => GatherRead::Cache,
    }
}
