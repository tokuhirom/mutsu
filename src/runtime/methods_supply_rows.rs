//! The one implementation behind `Supply`'s rows (ADR-11276 §9.130,
//! `method_table/instances/supply.rs`): each method of a `Supply` instance
//! goes to the routine that answered it by name before, so a call reaches the
//! same answer whatever the receiver is (a variable, a temporary) and from
//! whichever dispatch entry. The subscribing methods (`tap`, `act`,
//! `on-close`) commit through the instance's shared cell, as every native
//! mutating method does.

use super::*;

/// A method of `Supply`, as its rows name it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SupplyOp {
    Map,
    Grep,
    First,
    Reduce,
    Produce,
    Elems,
    Min,
    Max,
    Minmax,
    Grab,
    Skip,
    Live,
    Collate,
    Delayed,
    Tap,
    Act,
    OnClose,
    Serialize,
    Sanitize,
    Merge,
    Zip,
    ZipLatest,
    Rotor,
    Squish,
    Sort,
    Head,
    Flat,
    Batch,
    Throttle,
    Comb,
    Words,
    Wait,
    Stable,
    Rotate,
    Share,
    Migrate,
    Encode,
    Decode,
    Tail,
    Split,
    Reverse,
    Repeated,
    Unique,
    Do,
    Promise,
    ScheduleOn,
    Lines,
    Classify,
    Categorize,
    Channel,
    Supply,
}

impl SupplyOp {
    /// The method's name, for the routines that take it.
    // Cost: O(1).
    pub(crate) fn name(self) -> &'static str {
        match self {
            SupplyOp::Map => "map",
            SupplyOp::Grep => "grep",
            SupplyOp::First => "first",
            SupplyOp::Reduce => "reduce",
            SupplyOp::Produce => "produce",
            SupplyOp::Elems => "elems",
            SupplyOp::Min => "min",
            SupplyOp::Max => "max",
            SupplyOp::Minmax => "minmax",
            SupplyOp::Grab => "grab",
            SupplyOp::Skip => "skip",
            SupplyOp::Live => "live",
            SupplyOp::Collate => "collate",
            SupplyOp::Delayed => "delayed",
            SupplyOp::Tap => "tap",
            SupplyOp::Act => "act",
            SupplyOp::OnClose => "on-close",
            SupplyOp::Serialize => "serialize",
            SupplyOp::Sanitize => "sanitize",
            SupplyOp::Merge => "merge",
            SupplyOp::Zip => "zip",
            SupplyOp::ZipLatest => "zip-latest",
            SupplyOp::Rotor => "rotor",
            SupplyOp::Squish => "squish",
            SupplyOp::Sort => "sort",
            SupplyOp::Head => "head",
            SupplyOp::Flat => "flat",
            SupplyOp::Batch => "batch",
            SupplyOp::Throttle => "throttle",
            SupplyOp::Comb => "comb",
            SupplyOp::Words => "words",
            SupplyOp::Wait => "wait",
            SupplyOp::Stable => "stable",
            SupplyOp::Rotate => "rotate",
            SupplyOp::Share => "share",
            SupplyOp::Migrate => "migrate",
            SupplyOp::Encode => "encode",
            SupplyOp::Decode => "decode",
            SupplyOp::Tail => "tail",
            SupplyOp::Split => "split",
            SupplyOp::Reverse => "reverse",
            SupplyOp::Repeated => "repeated",
            SupplyOp::Unique => "unique",
            SupplyOp::Do => "do",
            SupplyOp::Promise => "Promise",
            SupplyOp::ScheduleOn => "schedule-on",
            SupplyOp::Lines => "lines",
            SupplyOp::Classify => "classify",
            SupplyOp::Categorize => "categorize",
            SupplyOp::Channel => "Channel",
            SupplyOp::Supply => "Supply",
        }
    }
}

impl Interpreter {
    /// Run `op` on `target`, a `Supply` instance (or, for the class methods
    /// `merge`, `zip` and `zip-latest`, the `Supply` type object). `None` for
    /// any other receiver.
    // Cost: the routine's own.
    pub(crate) fn supply_call(
        &mut self,
        target: &Value,
        op: SupplyOp,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let attributes = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => attributes,
            ValueView::Package(name) if name == "Supply" => {
                return match op {
                    SupplyOp::Merge => Some(self.dispatch_supply_merge(&args)),
                    SupplyOp::Zip => Some(self.dispatch_supply_zip_class(&args)),
                    SupplyOp::ZipLatest => Some(self.dispatch_supply_zip_latest_class(&args)),
                    // `grab` is an instance method; the type object refuses it.
                    SupplyOp::Grab => self.dispatch_grab_method(target.clone(), args),
                    _ => None,
                };
            }
            _ => return None,
        };
        let target = target.clone();
        Some(match op {
            SupplyOp::Map => self.dispatch_supply_map(&target, &attributes.as_map(), &args),
            SupplyOp::Grep => self.dispatch_grep_method(target, args)?,
            SupplyOp::First => self.dispatch_first(target, &args),
            SupplyOp::Reduce => match args.first() {
                Some(callable) => {
                    let callable = callable.clone();
                    self.dispatch_supply_reduce(target, &attributes.to_map(), callable)
                }
                None => Err(RuntimeError::new("reduce expects a callable")),
            },
            SupplyOp::Elems => self.dispatch_supply_elems(&attributes.as_map(), &args),
            SupplyOp::Min | SupplyOp::Max => {
                self.dispatch_supply_running_extrema(target, op.name(), &args)
            }
            // A mapper is a transform; the running `minmax` of the values
            // themselves is the native supply's.
            SupplyOp::Minmax if args.is_empty() => {
                self.native_supply(&attributes.as_map(), op.name(), args)
            }
            SupplyOp::Minmax => self.dispatch_minmax_method(target, op.name(), args)?,
            SupplyOp::Grab => self.dispatch_supply_grab(&attributes.as_map(), &args),
            SupplyOp::Skip => self.dispatch_supply_skip(&attributes.as_map(), &args),
            SupplyOp::Live => self.native_supply(&attributes.as_map(), op.name(), Vec::new()),
            SupplyOp::Collate => self.dispatch_collate(target),
            // `.delayed(0)` (or less) is the supply itself.
            SupplyOp::Delayed if args.first().is_some_and(|delay| delay.to_f64() <= 0.0) => {
                Ok(target)
            }
            SupplyOp::Tap | SupplyOp::Act | SupplyOp::OnClose => {
                self.call_native_instance_method_mut_in_place(&attributes, "Supply", op.name(), args)
            }
            // A synchronous supply is already serial and sane.
            SupplyOp::Serialize | SupplyOp::Sanitize => Ok(target),
            _ => self.dispatch_supply_transform(target, op.name(), &args),
        })
    }

    /// `Supply.list` / `.Array`: a materialized supply's values (the pure
    /// `listify`), else a live, channel-backed supply (an `IO::Socket::Async`
    /// read stream) drained until the producer signals done, else the values
    /// the stateful path collects (an on-demand supply runs its body). Only
    /// this entry point blocks: the combinators stay lazy on a live source,
    /// or `Supply.interval(.1).map({ … })` would wait out a stream that never
    /// ends. `None` for anything but a `Supply` instance.
    // Cost: O(e), e = values the supply emits until done.
    pub(crate) fn supply_materialize(
        &mut self,
        target: &Value,
        want_array: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if class_name != "Supply" {
            return None;
        }
        if let Some(result) = crate::builtins::method_table::supply::listify(target, want_array) {
            return Some(result);
        }
        let attrs = attributes.as_map();
        if !want_array && Self::supply_lists_lazily(&attrs) {
            return Some(self.supply_lazy_values(target, false));
        }
        if !want_array && let Some(list) = Self::supply_native_lazy_values(&attrs, false) {
            return Some(Ok(list));
        }
        if !attrs.contains_key("proc_output")
            && let Some(sid) = attrs.get("supply_id").and_then(Value::as_int)
            && let Some(rx) = crate::runtime::native_methods::take_supply_channel(sid as u64)
        {
            let mut items = Vec::new();
            while let Ok(crate::runtime::native_methods::SupplyEvent::Emit(value)) = rx.recv() {
                items.push(value);
            }
            return Some(Ok(Value::array(items)));
        }
        // A supply derived from a channel-backed live one (`Supply.interval(..)
        // .head(N)`, `.grep`, `.map`) has no channel or values of its own: tap it
        // and collect what it emits until it is done.
        if !attrs.contains_key("proc_output") && attrs.contains_key("parent_supply_id") {
            return Some(self.supply_collect_values(target, true).map(|items| {
                if want_array {
                    crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
                } else {
                    Value::array(items)
                }
            }));
        }
        Some(self.supply_list_values(&attrs, true).map(Value::array))
    }

    /// Whether `.list` / `.Seq` of a supply with these attributes taps it
    /// and pulls lazily ([`Self::supply_lazy_values`]): an on-demand
    /// `supply { }` block and a supply derived from a live one
    /// (`Supply.interval(1).map(...)`). A static supply (`Supply.from-list`) is
    /// answered from its stored values by `listify` before this is asked.
    /// A supply backed directly by a native event channel (`supply_id`) is not
    /// tapped: its `.tap` does not report `done` for every source (the read
    /// stream of an already closed socket), so its lazy list pulls the channel
    /// itself ([`Self::supply_native_lazy_values`]).
    // Cost: O(1).
    pub(crate) fn supply_lists_lazily(attrs: &crate::value::AttrMap) -> bool {
        !attrs.contains_key("proc_output")
            && (attrs.contains_key("on_demand_callback") || attrs.contains_key("parent_supply_id"))
    }

    /// `Supply.list` / `.Seq` (Rakudo: "taps the Supply it is called on, and
    /// returns a lazy list that will be reified as the Supply emits values"):
    /// tap `supply` with collector shims that feed a channel, and answer the
    /// channel's lazy list. Each element is pulled as it is emitted, so
    /// `for $supply.list { last if ... }` ends without waiting for the
    /// supply to be done (a `whenever Supply.interval(60)` in the block
    /// never is); `done` ends the list and a `quit` is thrown once the values
    /// emitted before it have been read. A list abandoned early leaves the
    /// tap open, as on Rakudo.
    // Cost: O(1) plus the synchronous part of the tap; O(1) per pulled element.
    pub(crate) fn supply_lazy_values(
        &mut self,
        supply: &Value,
        seq: bool,
    ) -> Result<Value, RuntimeError> {
        let ch = self.supply_tap_into_channel(supply)?;
        let mut ll = crate::value::LazyList::new_cat_pull(
            Value::channel(ch),
            crate::value::CatPullMode::ChannelReceive,
        );
        ll.list_context = !seq;
        Ok(Value::lazy_list(crate::gc::Gc::new(ll)))
    }

    /// Tap `supply` with collector shims that feed a fresh channel: emit
    /// sends, done closes, quit fails it (Rakudo's `Supply.Channel`).
    // Cost: O(1) plus the synchronous part of the tap.
    pub(crate) fn supply_tap_into_channel(
        &mut self,
        supply: &Value,
    ) -> Result<crate::value::SharedChannel, RuntimeError> {
        let ch = crate::value::SharedChannel::new();
        let collector_id =
            crate::runtime::native_methods::register_supply_channel_collector(ch.clone());
        let emit_shim = Self::build_supply_collector_shim(collector_id, "emit");
        let done_shim = Self::build_supply_collector_shim(collector_id, "done");
        let quit_shim = Self::build_supply_collector_shim(collector_id, "quit");
        if let Err(err) = self.call_method_with_values(
            supply.clone(),
            "tap",
            vec![
                emit_shim,
                Value::pair("done".to_string(), done_shim),
                Value::pair("quit".to_string(), quit_shim),
            ],
        ) {
            crate::runtime::native_methods::unregister_supply_collector(collector_id);
            return Err(err);
        }
        Ok(ch)
    }

    /// `.list` / `.Seq` of a supply backed directly by a native event channel
    /// (`supply_id`: `Supply.interval`, an `IO::Socket::Async` read stream):
    /// subscribe to the channel and answer a lazy list that pulls one event
    /// per element (`CatPullMode::SupplyReceive`), ending at the stream's
    /// done. The eager drain this replaces never returned for a supply that is
    /// never done (`Supply.interval(1).list`, #17428). `None` when the supply
    /// has no such channel.
    // Cost: O(1); O(1) per pulled element plus the wait for it.
    pub(crate) fn supply_native_lazy_values(
        attrs: &crate::value::AttrMap,
        seq: bool,
    ) -> Option<Value> {
        if attrs.contains_key("proc_output") {
            return None;
        }
        let sid = attrs.get("supply_id").and_then(Value::as_int)?;
        let rx = crate::runtime::native_methods::take_supply_channel(sid as u64)?;
        let pull_id = crate::runtime::native_methods::register_supply_pull(rx);
        let mut ll = crate::value::LazyList::new_cat_pull(
            Value::int(pull_id as i64),
            crate::value::CatPullMode::SupplyReceive,
        );
        ll.list_context = !seq;
        Some(Value::lazy_list(crate::gc::Gc::new(ll)))
    }
}
