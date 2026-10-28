//! `Any`'s methods on a receiver that brings its own `iterator` or `list`
//! (ADR-11276 §9.148).
//!
//! Rakudo writes `Any`'s list methods on top of `self.iterator` (`head`,
//! `map`, `sort`, `sum`, ...) or `self.list` (`join`, `elems`, `kv`, `flat`,
//! ...), and `Any.iterator` is `self.list.iterator`. So a receiver that
//! declares `iterator` answers the first group from its elements, and one that
//! declares `list` answers both groups from its list; neither needs to compose
//! `Iterable`. The method sets were measured against Rakudo 2026.09 on a plain
//! class declaring only `iterator` and on one declaring only `list`.
//!
//! The built-in classes mutsu keeps as an instance follow the same rule:
//! `Backtrace` and a buffer-backed `CArray` have a `list` of their own, an
//! `IterationBuffer` an `iterator`. A receiver that declares the method itself
//! is not redirected.

use super::*;

use crate::builtins::method_table::ANY_VIA_ITERATOR as VIA_ITERATOR;

/// `Any`'s methods that read `self.list` (besides those of [`VIA_ITERATOR`],
/// which reach it through `Any.iterator`).
const VIA_LIST: [&str; 21] = [
    "join",
    "elems",
    "reverse",
    "kv",
    "pairs",
    "keys",
    "Array",
    "List",
    "list",
    "cache",
    "eager",
    "flat",
    "Slip",
    "classify",
    "categorize",
    "combinations",
    "permutations",
    "end",
    "antipairs",
    "maxpairs",
    "minpairs",
];

/// Where a receiver's elements come from.
enum Source {
    /// Its own `iterator`: the [`VIA_ITERATOR`] methods, and `flat` when it
    /// is `Iterable` (the role declares `flat` on the iterator). A `Sequence`
    /// also answers the [`VIA_LIST`] methods: the role's `list` is its iterator.
    Iterator { iterable: bool, sequence: bool },
    /// Its own `list`: those and the [`VIA_LIST`] methods.
    List,
}

impl Source {
    /// Whether `method` reads the elements this source gives.
    // Cost: O(m), m = names in the two sets.
    fn reads(&self, method: &str) -> bool {
        match self {
            Source::Iterator { iterable, sequence } => {
                VIA_ITERATOR.contains(&method)
                    || (*iterable && method == "flat")
                    || (*sequence && VIA_LIST.contains(&method))
            }
            Source::List => VIA_ITERATOR.contains(&method) || VIA_LIST.contains(&method),
        }
    }
}

impl Interpreter {
    /// Whether `Any`'s `method` on an instance of the user class `class` reads
    /// the class's own `iterator` or `list` ([`Self::own_iteration_view`]
    /// answers it), so the method table must not answer it as one item.
    // Cost: memoized method probes plus O(m), m = names in the two sets.
    pub(crate) fn own_iteration_reads(&mut self, class: &str, method: &str) -> bool {
        !self.has_user_method(class, method)
            && self
                .class_source(class)
                .is_some_and(|source| source.reads(method))
    }

    /// Whether `target` is buffer-backed storage whose class declares its own
    /// `list` (upstream's `CArray`), so `Any`'s `method` reads that `list`
    /// ([`Self::own_iteration_view`] answers it) and no native entry may.
    // Cost: O(m) name scan plus memoized method probes, m = names in the two sets.
    pub(crate) fn buf_own_list_reads(&mut self, target: &Value, method: &str) -> bool {
        if !VIA_ITERATOR.contains(&method) && !VIA_LIST.contains(&method) && method != "flat" {
            return false;
        }
        let Some((class, _)) = crate::value::value_buf::buf_target(target) else {
            return false;
        };
        let class = class.resolve();
        !self.mixin_composes_method(target, method)
            && !self.has_user_method(&class, method)
            && self.has_user_method(&class, "list")
    }

    /// The answer of `method` for a receiver with its own `iterator` or `list`,
    /// redispatched on its elements; `None` when `method` reads neither, the
    /// receiver has neither, or it declares `method` itself.
    // Cost: O(1) to decide (short list scans and memoized method probes);
    // O(n) to collect n elements, plus the method on them.
    pub(super) fn own_iteration_view(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !VIA_ITERATOR.contains(&method) && !VIA_LIST.contains(&method) && method != "flat" {
            return None;
        }
        let source = self.iteration_source(target, method)?;
        if !source.reads(method) {
            return None;
        }
        let elements = match self.own_elements(target, &source) {
            Ok(elements) => elements,
            Err(err) => return Some(Err(err)),
        };
        // `.head(n)` / `.first` pull only the prefix they need from the Seq.
        let elements = match self.take_seq_prefix(&elements, method, args) {
            Ok(Some(prefix)) => prefix,
            Ok(None) => elements,
            Err(err) => return Some(Err(err)),
        };
        Some(self.call_method_with_values(elements, method, args.to_vec()))
    }

    /// The source of a user class's elements: its own `list`, else its own
    /// `iterator`.
    // Cost: memoized method probes.
    fn class_source(&mut self, class: &str) -> Option<Source> {
        if self.has_user_method(class, "list") {
            return Some(Source::List);
        }
        self.has_user_method(class, "iterator")
            .then(|| Source::Iterator {
                iterable: self.class_does_role(class, "Iterable"),
                sequence: self.class_does_role(class, "Sequence"),
            })
    }

    /// Whether `target` brings its own `iterator` or `list` for `method`.
    // Cost: O(1) plus memoized method probes.
    fn iteration_source(&mut self, target: &Value, method: &str) -> Option<Source> {
        // A buffer's elements are its own; it declares `elems`, `list` and
        // the other `self.list` methods itself.
        if let Some((class, _)) = crate::value::value_buf::buf_target(target) {
            if self.mixin_composes_method(target, method)
                || self.has_user_method(&class.resolve(), method)
            {
                return None;
            }
            // A buffer-backed class that declares its own `list` (upstream's
            // `CArray`) answers `Any`'s `self.list` methods from it too.
            if self.has_user_method(&class.resolve(), "list") {
                return Some(Source::List);
            }
            return Some(Source::Iterator {
                iterable: true,
                sequence: false,
            });
        }
        match target.view() {
            ValueView::Mixin(inner, _) => {
                if self.mixin_composes_method(target, method) {
                    return None;
                }
                if self.mixin_composes_method(target, "iterator") {
                    return Some(Source::Iterator {
                        iterable: false,
                        sequence: false,
                    });
                }
                // A `Sequence` class's own `iterator` is the mixin's too.
                let ValueView::Instance { class_name, .. } = inner.view() else {
                    return None;
                };
                let class = class_name.resolve();
                if self.has_user_method(&class, method) {
                    return None;
                }
                match self.class_source(&class)? {
                    source @ Source::Iterator { sequence: true, .. } => Some(source),
                    _ => None,
                }
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                // An `is Array`/`is List` subclass iterates its storage
                // (`positional_subclass_iteration_source`).
                if attributes.contains_key("__mutsu_array_storage") {
                    return None;
                }
                let class = class_name.resolve();
                if self.has_user_method(&class, method) {
                    return None;
                }
                if class == "Backtrace" {
                    return Some(Source::List);
                }
                if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
                    return Some(Source::Iterator {
                        iterable: false,
                        sequence: false,
                    });
                }
                self.class_source(&class)
            }
            _ => None,
        }
    }

    /// The elements `source` gives for `target`, as a `List`.
    // Cost: O(1) for an `iterator` source (pulled on demand by the consumer),
    // O(n), n = elements, for a `list` source.
    fn own_elements(&mut self, target: &Value, source: &Source) -> Result<Value, RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        {
            if class_name == "Backtrace" {
                return Ok(attributes
                    .as_map()
                    .get("frames")
                    .cloned()
                    .unwrap_or_else(|| Value::array(Vec::new())));
            }
            if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
                let items = match attributes
                    .as_map()
                    .get(crate::runtime::nqp_ops_list::iteration_buffer_items_key())
                    .map(Value::view)
                {
                    Some(ValueView::Array(values, ..)) => values.to_vec(),
                    Some(ValueView::Seq(values)) => values.to_vec(),
                    Some(ValueView::Slip(values)) => values.to_vec(),
                    _ => Vec::new(),
                };
                return Ok(Value::array(items));
            }
        }
        match source {
            Source::List => {
                let list = self.call_method_with_values(target.clone(), "list", Vec::new())?;
                Ok(Value::array(crate::runtime::utils::value_to_list(&list)))
            }
            Source::Iterator { .. } if crate::value::value_buf::buf_target(target).is_some() => {
                let list = self.call_method_with_values(target.clone(), "list", Vec::new())?;
                Ok(Value::array(crate::runtime::utils::value_to_list(&list)))
            }
            // The class's own `iterator` becomes a deferred `Seq` over it, as
            // `Seq.new($iterator)`: Rakudo's `Any.map`/`head`/... pull from
            // `self.iterator` on demand, so an iterator that never ends
            // (`is-lazy`) must not be drained up front.
            Source::Iterator { .. } => {
                let iterator = if let Some(result) =
                    self.dispatch_mixin_method_call(target, "iterator", Vec::new())
                {
                    result?
                } else if matches!(target.view(), ValueView::Mixin(..)) {
                    // A class `iterator` under a mixin runs on the mixin, which
                    // delegates to the wrapped instance.
                    self.call_method_with_values(target.clone(), "iterator", Vec::new())?
                } else {
                    self.try_compiled_method_or_interpret(target.clone(), "iterator", vec![])?
                };
                let seq = self.try_native_seq_construct(&[iterator]);
                if let ValueView::Seq(body) = seq.view() {
                    // Ask the user iterator its `is-lazy`, so `.head(n)` pulls
                    // only `n` items from one that never ends.
                    self.resolve_seq_iterator_laziness(&body)?;
                }
                Ok(seq)
            }
        }
    }
}
