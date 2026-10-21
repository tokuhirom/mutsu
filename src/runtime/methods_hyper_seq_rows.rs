//! The routines behind the `HyperSeq`/`RaceSeq` rows and the `hyper`/`race`
//! twins that take `:batch`/`:degree` (ADR-11276 §9.137).

use super::*;

impl Interpreter {
    /// The elements of a hyper or race sequence, and whether it is a hyper
    /// one; `None` for anything else.
    // Cost: O(1).
    fn hyper_seq_body(target: &Value) -> Option<(std::sync::Arc<crate::value::SeqBody>, bool)> {
        match target.view() {
            ValueView::HyperSeq(items) => Some((items.clone(), true)),
            ValueView::RaceSeq(items) => Some((items.clone(), false)),
            _ => None,
        }
    }

    /// The sequence as a plain `List`, which every method a hyper or race
    /// sequence does not declare reads.
    // Cost: O(n), n = elements of the sequence.
    pub(crate) fn hyper_seq_as_list(target: &Value) -> Option<Value> {
        let (items, _) = Self::hyper_seq_body(target)?;
        Some(Value::array_with_kind(
            crate::value::Value::array_arc(items.to_vec()),
            crate::value::ArrayKind::List,
        ))
    }

    /// `.hyper` / `.race` on a hyper or race sequence: the same elements as
    /// the other flavour (or this one).
    // Cost: O(1).
    pub(crate) fn hyper_seq_rewrap(
        &mut self,
        target: &Value,
        hyper: bool,
    ) -> Result<Value, RuntimeError> {
        let (items, _) = Self::hyper_seq_body(target)
            .ok_or_else(|| RuntimeError::new("hyper/race on a non-hyper sequence"))?;
        Ok(if hyper {
            Value::hyper_seq_body(items)
        } else {
            Value::race_seq_body(items)
        })
    }

    /// `HyperSeq.configuration`: a `HyperConfiguration` exposing the
    /// `.batch`/`.degree` the sequence was hyperized with, or the core
    /// defaults when unspecified (the `hyperize` dist reads it).
    // Cost: O(1).
    pub(crate) fn hyper_seq_configuration(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let (items, _) = Self::hyper_seq_body(target)
            .ok_or_else(|| RuntimeError::new("configuration on a non-hyper sequence"))?;
        let (batch, degree) = items.hyper_config().unwrap_or((None, None));
        Ok(Self::make_hyper_configuration(batch, degree))
    }

    /// `HyperSeq.iterator`: a hyper or race sequence allows a single iterator
    /// (rakudo #4413), so a second `.iterator` throws `X::Seq::Consumed`,
    /// even though the body is reified from birth. The check and the mark are
    /// one atomic step, so concurrent workers racing for the iterator resolve
    /// to exactly one winner.
    // Cost: O(n), n = elements of the sequence.
    pub(crate) fn hyper_seq_iterator(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let (items, is_hyper) = Self::hyper_seq_body(target)
            .ok_or_else(|| RuntimeError::new("iterator on a non-hyper sequence"))?;
        if items.claim_hyper_iterator_once().is_err() {
            let type_name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
            return Err(crate::value::seq_consumed_error_for(type_name));
        }
        let list = Self::hyper_seq_as_list(target).unwrap_or(Value::NIL);
        Ok(crate::builtins::iterator_construct::build_iterator_instance(&list))
    }

    /// `HyperSeq.map` / `.grep`: the list's answer, kept a hyper (race)
    /// sequence. A small sequence runs its callbacks on the worker pool, so
    /// inter-item synchronization (a `Promise` await chain) works; a large
    /// one (1000 elements or more) takes the sequential list path, as the
    /// workers' batches would retain every expanded result until their join.
    // Cost: O(n), n = elements of the sequence (one callback call each).
    pub(crate) fn hyper_seq_map_grep(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (items, is_hyper) = Self::hyper_seq_body(target)
            .ok_or_else(|| RuntimeError::new("map/grep on a non-hyper sequence"))?;
        if items.len() < 1000 {
            let block = args.first().cloned().unwrap_or(Value::NIL);
            let result =
                self.exec_hyper_race_map_grep(&items, block, method == "map", is_hyper)?;
            return Ok(if is_hyper {
                Value::hyper_seq(result)
            } else {
                Value::race_seq(result)
            });
        }
        let list = Self::hyper_seq_as_list(target).unwrap_or(Value::NIL);
        let result = if method == "map" {
            self.dispatch_map_method(list, args)?
        } else {
            self.dispatch_grep_method(list, args)
                .unwrap_or_else(|| Err(RuntimeError::new("grep declined")))?
        };
        // ADR-0058: the list's `.map`/`.grep` hands back a Seq whose callback
        // has not run; `value_to_list` reads it through pure code.
        self.reify_map_grep_seq(&result)?;
        let result_items = crate::runtime::utils::value_to_list(&result);
        // ... and so may each element, when the callback itself returned a
        // `.map`/`.grep` Seq: the hyper sequence is built eagerly from them.
        self.reify_map_grep_seq_args(&result_items)?;
        Ok(if is_hyper {
            Value::hyper_seq(result_items)
        } else {
            Value::race_seq(result_items)
        })
    }

    /// The `:batch` / `:degree` of a `.hyper` / `.race` call; a non-positive
    /// one is `X::Invalid::Value`.
    // Cost: O(a), a = arguments of the call.
    pub(crate) fn hyper_adverbs(
        method: &str,
        args: &[Value],
    ) -> Result<(Option<i64>, Option<i64>), RuntimeError> {
        let mut batch: Option<i64> = None;
        let mut degree: Option<i64> = None;
        for arg in args {
            let (key, val) = match arg.view() {
                ValueView::Pair(k, v) => (k.to_string(), crate::runtime::to_int(v)),
                ValueView::ValuePair(k, v) => (k.to_string_value(), crate::runtime::to_int(v)),
                _ => continue,
            };
            if key == "batch" {
                batch = Some(val);
            } else if key == "degree" {
                degree = Some(val);
            }
        }
        for (name, value) in [("batch", batch), ("degree", degree)] {
            if let Some(v) = value
                && v <= 0
            {
                let mut attrs = ValueMap::default();
                attrs.insert("method".to_string(), Value::str(method.to_string()));
                attrs.insert("name".to_string(), Value::str(name.to_string()));
                attrs.insert("value".to_string(), Value::int(v));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Invalid value '{v}' for :{name} on method {method}")),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
        }
        Ok((batch, degree))
    }

    /// `.hyper(:$batch, :$degree)` / `.race(...)` on a list: the adverbs
    /// validated (a non-positive one is `X::Invalid::Value`) and remembered
    /// for `.configuration`.
    // Cost: O(n), n = elements of the receiver.
    pub(crate) fn hyperize_with_config(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (batch, degree) = Self::hyper_adverbs(method, args)?;
        if let ValueView::LazyList(ll) = target.view()
            && ll.renders_lazy_placeholder()
        {
            let kind = if method == "hyper" {
                crate::value::HyperKind::Hyper
            } else {
                crate::value::HyperKind::Race
            };
            return Ok(Value::lazy_list(crate::gc::Gc::new(
                ll.with_hyper_kind(kind, Some((batch, degree))),
            )));
        }
        let body = crate::value::SeqBody::reified(crate::runtime::utils::value_to_list(target));
        body.set_hyper_config(batch, degree);
        Ok(if method == "hyper" {
            Value::hyper_seq_body(body)
        } else {
            Value::race_seq_body(body)
        })
    }
}
