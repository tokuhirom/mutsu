//! ADR-0040's store boundary, `Proxy` half.
//!
//! ADR-0040 decided that an `Array`/`Hash` element is a `Scalar` container, and
//! that mutsu makes that true at the *store* rather than compensating at the
//! read. A `Proxy` asks the same question and gets the same answer: raku reads
//! the RHS of `=` in value context, so a `Proxy` that lands inside a container
//! is FETCHed on the way in and the element that lands is a plain value.
//!
//! See ADR-0040 §9 for the boundary's full site list and the two deliberate
//! exemptions (`:=` binds, and `List` elements — which are not containers).

use super::*;

impl Interpreter {
    /// ADR-0040's store boundary, `Proxy` half: a `Proxy` that lands *inside* a
    /// container — a `$` `Scalar`, an `Array` element, a `Hash` value — is
    /// FETCHed on the way in, because raku reads the RHS of `=` in value
    /// context. The element that lands is a plain value, so a later mutation of
    /// the Proxy's backing lexical no longer changes it, and an `is rw` alias to
    /// that element writes the ARRAY rather than firing the Proxy's `STORE`.
    ///
    /// Two things must NOT come through here. A `:=` bind installs the `Proxy`
    /// itself (that is what keeps `$p.VAR.^name` a `Proxy` and makes `$p = 1`
    /// call `STORE`), and a `List`'s elements are not containers, so
    /// `my $l = (1, $p, 3)` keeps the Proxy and re-FETCHes it on every read.
    pub(crate) fn fetch_proxy_for_store(&mut self, value: Value) -> Result<Value, RuntimeError> {
        // Tag probe first: the overwhelmingly common non-Proxy store pays one
        // discriminant test and no clone.
        if !value.is_proxy_value() {
            return Ok(value);
        }
        self.auto_fetch_proxy(&value)
    }

    /// Whether a container-mutator argument carries a `Proxy` that the store
    /// will land in an element container — the argument itself, or the value
    /// half of a `Pair` (`%h.push(k => $p)` stores that value in `%h<k>`).
    pub(crate) fn arg_carries_store_proxy(arg: &Value) -> bool {
        match arg.view() {
            ValueView::Proxy { .. } => true,
            ValueView::Pair(_, v) | ValueView::ValuePair(_, v) => v.is_proxy_value(),
            _ => false,
        }
    }

    /// [`Self::fetch_proxy_for_store`] for a container-mutator argument,
    /// reaching through a `Pair` to its value half — see
    /// [`Self::arg_carries_store_proxy`].
    pub(crate) fn fetch_proxy_in_store_arg(&mut self, arg: Value) -> Result<Value, RuntimeError> {
        match arg.view() {
            ValueView::Pair(k, v) if v.is_proxy_value() => {
                let fetched = self.auto_fetch_proxy(v)?;
                Ok(Value::pair(k.clone(), fetched))
            }
            ValueView::ValuePair(k, v) if v.is_proxy_value() => {
                let fetched = self.auto_fetch_proxy(v)?;
                Ok(Value::value_pair(k.clone(), fetched))
            }
            _ => self.fetch_proxy_for_store(arg),
        }
    }

    /// ADR-0040's store boundary, Proxy half, for a container mutator's
    /// arguments: every element `push`/`unshift`/`append`/`prepend`/`splice`
    /// stores is a `Scalar` container, so a `Proxy` argument is FETCHed on the
    /// way in exactly as `@a[0] = $p` is. Scoped to those methods — a method
    /// call is NOT a general FETCH boundary, and this sits on the hot dispatch
    /// path — and a no-op unless an argument really carries a Proxy.
    pub(crate) fn fetch_proxy_mutator_args(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !matches!(method, "push" | "unshift" | "append" | "prepend" | "splice")
            || !args.iter().any(Self::arg_carries_store_proxy)
        {
            return Ok(args);
        }
        let mut fetched = Vec::with_capacity(args.len());
        for arg in args {
            fetched.push(self.fetch_proxy_in_store_arg(arg)?);
        }
        Ok(fetched)
    }

    /// The element-wise twin of [`Self::fetch_proxy_for_store`], for a whole
    /// container store or construction (`my @a = (1, $p, 3)`, `[1, $p]`).
    /// Discriminated by `ArrayKind` exactly like `itemize_real_array_elements`:
    /// only a REAL `Array`'s elements (and a `Hash`'s values) are containers.
    /// Scan-then-rebuild-only-if-needed, so a Proxy-free container is untouched
    /// and keeps sharing its `Gc`.
    pub(crate) fn fetch_proxy_container_elements(
        &mut self,
        mut value: Value,
    ) -> Result<Value, RuntimeError> {
        let needs = match value.view() {
            ValueView::Array(items, kind) if kind.is_real_array() => {
                items.iter().any(Value::is_proxy_value)
            }
            ValueView::Hash(map) => map.values().any(Value::is_proxy_value),
            _ => false,
        };
        if !needs {
            return Ok(value);
        }
        // FETCH first, mutate after: a FETCH body runs arbitrary user code that
        // may itself reach this container, so nothing may hold it borrowed.
        match value.view() {
            ValueView::Array(items, _) => {
                let sources: Vec<Value> = items.iter().cloned().collect();
                let mut fetched = Vec::with_capacity(sources.len());
                for item in sources {
                    fetched.push(self.fetch_proxy_for_store(item)?);
                }
                value.with_array_mut(|items, _kind| {
                    let data = crate::gc::Gc::make_mut(items);
                    for (slot, new) in data.items_mut().iter_mut().zip(fetched) {
                        *slot = new;
                    }
                });
            }
            ValueView::Hash(map) => {
                let sources: Vec<(String, Value)> =
                    map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                let mut fetched = Vec::with_capacity(sources.len());
                for (k, v) in sources {
                    if v.is_proxy_value() {
                        let v = self.fetch_proxy_for_store(v)?;
                        fetched.push((k, v));
                    }
                }
                value.with_hash_mut(|data| {
                    let data = crate::gc::Gc::make_mut(data);
                    for (k, v) in fetched {
                        data.map.insert(k, v);
                    }
                });
            }
            _ => {}
        }
        Ok(value)
    }
}
