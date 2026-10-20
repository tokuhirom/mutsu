//! The interpreter half of a plain `gather` as a `Seq`
//! (docs/adr/11678-gather-is-a-deferred-seq-source.md): driving the
//! `SeqSource::Gather` coroutine a prefix at a time, and reading a gather
//! argument the way the builtin routine or method it is passed to reads it.

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

    /// Elements `from..needed` of a gather's inner coroutine list (fewer when
    /// it finishes first), running the body only as far as that takes. The
    /// pull [`crate::value::SeqBody::extend_gather_prefix`] appends.
    // Cost: O(needed - from) for the copy, plus running the body to its
    // `needed`-th `take`.
    pub(crate) fn pull_gather_window(
        &mut self,
        list: &Value,
        from: usize,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        match list.view() {
            ValueView::LazyList(ll) => self.force_lazy_list_vm_window(&ll, from, needed),
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

    /// The operand of a reduction `[op] value`, read as
    /// [`Self::iterated_operand`] reads it: the triangle form (`scan`)
    /// streams a gather, so `([\+] gather { loop {...} })[^5]` answers, and
    /// the plain form pulls it whole through the body. `[max]` / `[min]`
    /// differ: they call the operator's list candidate, whose `+args`
    /// binding caches a Seq instead of consuming it (`[max] s; s.elems`
    /// answers in Rakudo), while a consumed one still raises.
    // Cost: as `iterated_operand`, or pulling the whole Seq for `[max]` /
    // `[min]`.
    pub(crate) fn reduction_operand(
        &mut self,
        value: Value,
        op: &str,
        scan: bool,
    ) -> Result<Value, RuntimeError> {
        if !scan
            && matches!(op, "max" | "min")
            && let Some(body) = Self::plain_seq_body(&value, true)
        {
            self.reify_seq_body(&body)?;
            return Ok(value);
        }
        self.iterated_operand(value, scan, true)
    }

    /// An operand an operator iterates (a reduction, the zip and cross
    /// metaoperators), read the way Rakudo's iterating operators read it: an
    /// uncached `Seq` is consumed (`[+] s; s.elems` and `s Z 1; s.elems`
    /// raise `X::Seq::Consumed`) and a consumed one raises at once. A plain
    /// gather is pulled whole through its body like any other Seq, so the
    /// inner coroutine list stays private (ADR-11678 §2.2), unless `stream`
    /// is set: then it is handed over as its coroutine list through
    /// [`Self::gather_operand`] (consuming the Seq), for the consumers that
    /// pull an infinite gather on demand. A lazy non-gather Seq
    /// (`Seq.new($iterator)`) and every non-`Seq` value are returned
    /// unchanged, for the lazy paths below. A `$`-itemized Seq is iterated
    /// only when `item` is set; otherwise it is one element, left alone.
    // Cost: O(1) for a non-Seq, a lazy Seq or a streamed gather; pulling the
    // whole Seq otherwise.
    pub(crate) fn iterated_operand(
        &mut self,
        value: Value,
        stream: bool,
        item: bool,
    ) -> Result<Value, RuntimeError> {
        use crate::value::SeqTaken;
        let Some(body) = Self::plain_seq_body(&value, item) else {
            return Ok(value);
        };
        if body.is_consumed() {
            return Err(crate::value::seq_consumed_error());
        }
        if body.is_gather_source() {
            if stream {
                return Ok(self.gather_operand(value));
            }
        } else if body.is_lazy() {
            return Ok(value);
        }
        let (items, outcome) = self.take_seq_body(&body)?;
        Ok(match outcome {
            SeqTaken::Taken => Value::seq(items),
            SeqTaken::Served => value,
        })
    }

    /// The body of a `Seq` read as a sequence: a plain view, or a
    /// `$`-itemized one when `item` is set (a reduction iterates `[+] $s`,
    /// while `$s Z 1` zips it as one element). Never a `.List` view.
    // Cost: O(1).
    fn plain_seq_body(value: &Value, item: bool) -> Option<std::sync::Arc<crate::value::SeqBody>> {
        use crate::value::SeqView;
        if !value.is_seq_value() {
            return None;
        }
        match value.view() {
            ValueView::Seq(body)
                if matches!(body.view(), SeqView::Seq)
                    || (item && matches!(body.view(), SeqView::ItemSeq)) =>
            {
                Some(std::sync::Arc::clone(&body))
            }
            _ => None,
        }
    }

    /// `flat` over a gather's coroutine list (the §8.2 Stream class hands
    /// `flat` the stolen list): a `Seq` that pulls the list one element at a
    /// time and flattens each, descending into the nested Seqs and gathers it
    /// took. Rakudo's `flat` reads its input lazily, so an infinite gather
    /// works (`flat(gather { loop {...} }).head(3)`), and the result is a
    /// plain Seq, not a lazy one. The flattening pipe stays inside the Seq's
    /// adapter source, like a gather's own coroutine list.
    // Cost: O(1); each element is flattened when it is pulled.
    pub(crate) fn lazy_flat_gather(list: Value) -> Value {
        let pipe = crate::value::LazyList::new_index_pipe(list, crate::value::IndexTransform::Flat);
        Value::seq_deferred(crate::value::SeqSource::Gather(Value::lazy_list(
            crate::gc::Gc::new(pipe),
        )))
    }

    /// `.flat` on a plain gather: the gather's coroutine list flattened as it
    /// is pulled ([`Self::lazy_flat_gather`]), consuming the Seq as Rakudo's
    /// `.flat` does (a cached one stays readable). `None` for every other
    /// call, which dispatches as usual.
    // Cost: O(1).
    pub(crate) fn gather_flat_method(
        &self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if method != "flat" || !args.is_empty() || !Self::is_gather_seq(target) {
            return None;
        }
        let list = self.gather_operand(target.clone());
        matches!(list.view(), ValueView::LazyList(_)).then(|| Self::lazy_flat_gather(list))
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
        self.read_gather_args(builtin_gather_read(name), args)
            .map(std::borrow::Cow::Owned)
    }

    /// The arguments of method `method` on `target` with every plain gather
    /// `Seq` read the way Rakudo's method reads it ([`method_gather_read`]),
    /// when the method is a built-in one: a user object's method, a role's,
    /// and a method the program declared on a type object keep receiving the
    /// Seq itself. Returns `args` unchanged when no argument is a gather.
    // Cost: O(n), n = arguments, plus running each gather the method reads
    // whole to completion.
    pub(crate) fn method_gather_args(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !Self::has_builtin_seq_read(&args) {
            return Ok(args);
        }
        let builtin = match target.view() {
            ValueView::Instance { .. } | ValueView::Mixin(..) => false,
            ValueView::Package(name) => {
                !self.has_user_method_including_role(&name.resolve(), method)
            }
            _ => true,
        };
        if !builtin {
            return Ok(args);
        }
        self.read_gather_args(method_gather_read(target, method), &args)
    }

    /// Every argument of `args` read as `read` says.
    // Cost: O(n), n = arguments, plus the reads.
    fn read_gather_args(
        &mut self,
        read: GatherRead,
        args: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            // A Seq an earlier read consumed (`sum(s); sum(s)`) cannot be
            // iterated again, whatever its source was. A routine that is not
            // known to iterate it (`infix:<~~>` against a type) may still take
            // it.
            if matches!(read, GatherRead::Stream | GatherRead::Consume)
                && Self::is_consumed_seq(arg)
            {
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
        Ok(out)
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
        // `cache` returns a List that pulls the gather as it is read.
        "list" | "item" | "defined" | "push" | "unshift" | "cache" => GatherRead::Pass,
        "first" | "zip" | "roundrobin" | "flat" | "map" | "grep" => GatherRead::Stream,
        "sum" | "min" | "max" | "minmax" | "unique" | "repeated" | "squish" | "pick" | "roll"
        | "classify" | "categorize" | "produce" | "reduce" | "keys" | "values" | "kv" | "pairs"
        | "permutations" | "combinations" | "cross" | "append" | "prepend" => GatherRead::Consume,
        _ => GatherRead::Cache,
    }
}

/// [`GatherRead`] for built-in method `method` on `target`, measured against
/// Rakudo (`t/collections/lazy-seq/gather-receiver-args.t`). Unlisted methods
/// read through the cache.
// Cost: O(1).
fn method_gather_read(target: &Value, method: &str) -> GatherRead {
    match method {
        // `List.new(g)` keeps the Seq readable (Rakudo stores it as one
        // element).
        "new" if matches!(target.view(), ValueView::Package(name) if name == "List") => {
            GatherRead::Cache
        }
        // `Supply.from-list` pulls the gather itself, so that a gather that
        // dies quits the supply; `polymod` pulls only the divisors the
        // decomposition reaches, leaving the Seq readable.
        "push" | "unshift" | "from-list" | "polymod" => GatherRead::Pass,
        "new" | "append" | "prepend" => GatherRead::Consume,
        _ => GatherRead::Cache,
    }
}
