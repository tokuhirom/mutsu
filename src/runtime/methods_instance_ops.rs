use super::methods_signature_errors::{
    make_method_not_found_error, make_multi_no_match_error, make_private_permission_error,
    make_private_unqualified_error,
};
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueMap;
use crate::value::ValueView;
use crate::value::types::is_stash_class_name;

/// The methods whose last resort is a late row of the receiver's MRO
/// (ADR-11276 §9.138).
const MU_LATE_BY_MRO: [&str; 11] = [
    "DUMP", "gist", "raku", "isa", "REPR", "Str", "Stringy", "Int", "Numeric", "Real",
    "EVAL",
];

impl Interpreter {
    fn is_pod_block_instance(&mut self, value: &Value) -> bool {
        let ValueView::Instance { class_name, .. } = value.view() else {
            return false;
        };
        self.class_mro(&class_name.resolve())
            .iter()
            .any(|name| name.resolve() == "Pod::Block")
    }

    fn pod_gist(&mut self, target: &Value, level: usize) -> String {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return target.to_string_value();
        };
        let attrs = attributes.as_map();
        let mut rendered = format!("{}{}", " ".repeat(level), class_name.resolve());
        let mut pairs = Vec::new();
        for name in ["config", "name", "level", "caption", "type", "term"] {
            let Some(value) = attrs.get(name) else {
                continue;
            };
            if value.is_nil() || value.to_string_value().is_empty() {
                continue;
            }
            let value =
                if value.as_list_items().is_some() || matches!(value.view(), ValueView::Hash(..)) {
                    crate::builtins::methods_0arg::raku_repr::raku_value(value)
                } else {
                    value.to_string_value()
                };
            pairs.push(format!(
                ":{}({})",
                name,
                crate::builtins::methods_0arg::raku_repr::raku_value(&Value::str(value))
            ));
        }
        if !pairs.is_empty() {
            rendered.push_str(&format!("{{{}}}", pairs.join(", ")));
        }
        let is_table = class_name.resolve() == "Pod::Block::Table";
        if let Some(contents) = attrs.get("contents").and_then(Value::as_list_items) {
            for content in contents {
                rendered.push('\n');
                if self.is_pod_block_instance(content) {
                    rendered.push_str(&self.pod_gist(content, level + 2));
                } else {
                    let text = if is_table && content.as_list_items().is_some() {
                        crate::builtins::methods_0arg::raku_repr::raku_value(content)
                    } else {
                        content.to_string_value()
                    };
                    let indent = " ".repeat(level + 2);
                    rendered.push_str(
                        &text
                            .lines()
                            .map(|line| format!("{indent}{line}"))
                            .collect::<Vec<_>>()
                            .join("\n"),
                    );
                }
            }
        }
        rendered
    }

    /// Whether an unqualified private call is permitted *lexically*: the
    /// executing code's `self` is an instance of the owning class.
    ///
    /// `method_class_stack` reflects the *runtime* caller, which is wrong for a
    /// private call written in a closure: in `$.b.run: { self!secret }` the
    /// stack top is B (whose method invoked the closure) though the call is
    /// lexically inside A. Raku resolves private calls lexically, and the
    /// closure runs with its captured `self` — so an env `self` whose class is
    /// (or inherits from) the owner marks code that was written inside that
    /// class. DBDish's Pg StatementHandle BUILD calls `self!get-meta` inside a
    /// block passed to `$!parent.protect-connection` this way.
    pub(super) fn lexical_self_allows_private(&mut self, resolved_owner: &str) -> bool {
        let Some(self_cls) = self.lexical_self_class() else {
            return false;
        };
        let self_cls = self_cls.resolve();
        self_cls == resolved_owner
            || self
                .class_mro(&self_cls)
                .iter()
                .any(|s| s.resolve() == resolved_owner)
    }

    /// The strict sibling of [`Interpreter::lexical_self_allows_private`], for a
    /// **qualified** private call (`$o!Owner::meth`).
    ///
    /// The loose form accepts an env `self` that merely *inherits* from the
    /// owner, which is right for the unqualified spelling but wrong here:
    /// writing the owner's name out is exactly how a subclass tries to reach a
    /// parent's private method, and Raku refuses that unless the parent
    /// `trusts` it (`roast/integration/advent2011-day11.t`'s `Untrusty is Order`
    /// pins the `X::Method::Private::Permission`). Requiring the captured
    /// `self` to be an instance of the owner *itself* still identifies code
    /// written inside the owner — which is all the callback case needs — without
    /// handing a subclass an inheritance-shaped grant the trust check already
    /// denied.
    fn lexical_self_is_private_owner(&mut self, resolved_owner: &str) -> bool {
        self.lexical_self_class()
            .is_some_and(|self_cls| self_cls.resolve() == resolved_owner)
    }

    /// The class of the env `self`: an instance's class, or the type object
    /// itself when a method runs on one (`T.all` whose lazy `gather { self!all }`
    /// is pulled later, outside the method; Trie).
    // Cost: O(1).
    fn lexical_self_class(&self) -> Option<Symbol> {
        match self.env.get("self").map(Value::view) {
            Some(ValueView::Instance { class_name, .. }) => Some(class_name),
            Some(ValueView::Package(name)) => Some(name),
            Some(ValueView::Proxy {
                subclass: Some((name, _)),
                ..
            }) => Some(*name),
            _ => None,
        }
    }

    /// Mu's default `.gist`/`.raku`/`.perl` rendering for a plain user
    /// instance (`ClassName.new(...)`, with special cases for `is Array`
    /// subclasses, `ObjAt`/`ValueObjAt`, `X::AdHoc`, schedulers,
    /// `IO::Path::Parts`, `Stash`, `Backtrace::Frame`). Returns `None` when
    /// `method`/`args` don't qualify (e.g. `.Str`, or a non-empty arg list)
    /// or `target` isn't an Instance.
    ///
    /// Factored out of `dispatch_instance_and_fallback`'s no-user-override
    /// fast path below so `native_any_base_next_candidate`
    /// (`src/runtime/builtins_dispatch_next.rs`) can reach the SAME default
    /// when `callsame`/`nextsame` from inside a user `gist`/`raku` override
    /// exhausts the MRO — that caller deliberately bypasses the
    /// `has_user_method` gate the original site applies below (the whole
    /// point of that fallback is to reach this default despite the override
    /// existing).
    pub(crate) fn default_instance_repr(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target.view()
        else {
            return None;
        };
        // The `IterationEnd` sentinel renders as its name in every form, as
        // rakudo's `Mu.raku` / `Mu.gist` special-case it.
        if target_id == crate::value::ITERATION_END_ID
            && matches!(method, "raku" | "gist" | "Str")
            && args.is_empty()
        {
            return Some(Ok(Value::str_from("IterationEnd")));
        }
        let has_coercion_gist = target.does_check("Real")
            || target.does_check("Numeric")
            || target.does_check("Stringy")
            || self.has_user_method(&class_name.resolve(), "Bridge");
        let is_callable_instance = matches!(
            class_name.resolve().as_str(),
            "Method"
                | "Submethod"
                | "Sub"
                | "Routine"
                | "Block"
                | "Code"
                | "WhateverCode"
                | "Callable"
        );
        let gist_default = method == "gist" && !has_coercion_gist && !is_callable_instance;
        // `Mu.perl` is `self.raku` (ADR-11276 §9.152). The call entries resolve
        // `perl` to `raku` unless a user `perl` is in reach, so `perl` arrives
        // here only as the base candidate of such an override's `callsame`.
        if method == "perl" && args.is_empty() {
            return Some(self.call_method_with_values(target.clone(), "raku", vec![]));
        }
        if !((method == "raku" || gist_default) && args.is_empty()) {
            return None;
        }
        if let Some(base) = attributes.rakuast_base()
            && let ValueView::RakuAst(node) = base.view()
        {
            let class_key = class_name.resolve();
            let display_name = crate::value::user_facing_type_name(&class_key);
            return Some(Ok(Value::str(crate::rakuast::subclass_gist(
                display_name.as_ref(),
                node,
            ))));
        }
        // An `is Str` subclass delegates `.raku`/`.gist` to its string payload,
        // so `class Foo is Str {}; Foo.new(:value("hi")).raku` is `"hi"` —
        // mirrors the `is Array`/`is Hash` backing-storage delegation below.
        if let Some(payload) = attributes.as_map().get("__mutsu_str_value").cloned() {
            return Some(self.call_method_with_values(payload, method, vec![]));
        }
        if let Some(payload) = crate::builtins::native_base::numeric_payload_of(&attributes) {
            if let Some(rendered) =
                self.numeric_subclass_repr(target, &class_name.resolve(), &payload, method)
            {
                return Some(rendered);
            }
            return Some(self.call_method_with_values(payload, method, vec![]));
        }
        // An `is Version` subclass (#8070) delegates any method it does not
        // define itself to its native Version payload -- `.raku`, `.Str`, and
        // any other Version method a real `Version::Raku`/`Version::Nginx`
        // caller reaches without overriding.
        if let Some(payload) = attributes.as_map().get("__mutsu_version_value").cloned() {
            return Some(self.call_method_with_values(payload, method, vec![]));
        }
        // An `is Pair` subclass renders as its Pair payload.
        if let Some(payload) = attributes.as_map().get("__mutsu_pair_value").cloned() {
            return Some(self.call_method_with_values(payload, method, vec![]));
        }
        if let Some(storage) = attributes.as_map().get("__mutsu_array_storage").cloned() {
            return Some(self.call_method_with_values(storage, method, vec![]));
        }
        if let Some(storage) = attributes.as_map().get("__mutsu_hash_storage").cloned() {
            let rendered = self.call_method_with_values(storage.clone(), method, vec![]);
            // An `is Map` subclass names itself in its `.raku`/`.gist`.
            if matches!(method, "raku" | "gist")
                && let Ok(text) = &rendered
            {
                return Some(Ok(Value::str(
                    crate::value::raku_repr::rename_map_subclass_repr(
                        &class_name.resolve(),
                        &storage,
                        text.to_string_value(),
                    ),
                )));
            }
            return Some(rendered);
        }
        // An `IterationBuffer` (or an `is IterationBuffer` subclass, which
        // renders through the same method and so under the base type's name)
        // is its elements' List repr followed by `.IterationBuffer`:
        // `().IterationBuffer`, `(5,).IterationBuffer`,
        // `(1, "two").IterationBuffer`. `.gist` is the same text, since
        // rakudo renders the elements with `.raku` in both. A buffer that has
        // not been pushed to yet holds no storage, which reads as empty.
        if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
            let elements = attributes
                .as_map()
                .get(crate::runtime::nqp_ops_list::iteration_buffer_items_key())
                .and_then(|items| match items.view() {
                    ValueView::Array(data, _) => Some(data.to_vec()),
                    _ => None,
                })
                .unwrap_or_default();
            let list = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(elements)),
                crate::value::ArrayKind::List,
            );
            return Some(
                self.call_method_with_values(list, "raku", vec![])
                    .map(|repr| Value::str(format!("{}.IterationBuffer", repr.to_string_value()))),
            );
        }
        // A QuantHash (`is BagHash`/`is Set`/...) subclass renders under its own
        // type name: `ABH(a(2) b)` for `.gist`, `("a"=>2).ABH` for `.raku` —
        // see `runtime/quanthash_subclass.rs`.
        if let Some(storage) = attributes.as_map().get("__baggy_data__").cloned() {
            let class_key = class_name.resolve();
            let name = crate::value::user_facing_type_name(&class_key).to_string();
            if method == "gist" {
                return Some(Ok(Value::str(
                    crate::runtime::utils::setbagmix_gist_named(&storage, Some(&name))
                        .unwrap_or_else(|| crate::runtime::utils::gist_value(&storage)),
                )));
            }
            let inner = self.call_method_with_values(storage, "raku", vec![]);
            return Some(inner.map(|v| {
                let text = v.to_string_value();
                // The storage's own `.raku` ends in the builtin coercer
                // (`("a"=>2).BagHash`); swap in the subclass's name.
                match text.rfind('.') {
                    Some(dot) => Value::str(format!("{}.{}", &text[..dot], name)),
                    None => Value::str(text),
                }
            }));
        }
        // A bootstrap attribute of a built-in class (`Int.^attributes`) is a
        // `BOOTSTRAPATTR` in Rakudo, which has no public attributes to render.
        if matches!(method, "raku")
            && crate::runtime::attribute_identity::is_bootstrapattr(target)
        {
            return Some(Ok(Value::str_from("BOOTSTRAPATTR.new")));
        }
        if class_name == Symbol::intern("ObjAt") || class_name == Symbol::intern("ValueObjAt") {
            let which = attributes.as_map().objat_which().unwrap_or_default();
            if method == "gist" {
                return Some(Ok(Value::str(which)));
            }
            return Some(Ok(Value::str(format!(
                "{}.new(\"{}\")",
                class_name.resolve(),
                which
            ))));
        }
        if class_name.resolve() == "X::AdHoc"
            && (method == "raku")
            && let attr_map = attributes.as_map()
            && let Some(payload) = attr_map
                .get("payload")
                .or_else(|| attr_map.get("message"))
                .cloned()
        {
            let payload_raku = self
                .call_method_with_values(payload.clone(), "raku", vec![])
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| payload.to_string_value());
            return Some(Ok(Value::str(format!(
                "X::AdHoc.new(payload => {payload_raku})"
            ))));
        }
        // A grammar `token`/`rule`/`regex` method object (`.^lookup`,
        // `.^method_table`) gists as its declaration source.
        if method == "gist"
            && let Some(src) = attributes.as_map().get("__mutsu_regex_source")
        {
            return Some(Ok(src.clone()));
        }
        if method == "gist"
            && class_name.resolve() != "Pod::Block::Declarator"
            && self.is_pod_block_instance(target)
        {
            return Some(Ok(Value::str(self.pod_gist(target, 0))));
        }
        match class_name.resolve().as_str() {
            // A declarator block *is* its documentation text: rakudo renders
            // the `#|` leading and `#=` trailing comments (newline-joined) for
            // both `.Str` and `.gist`, so `say &foo.WHY` prints the doc rather
            // than the generic `TypeName.new` instance repr. `.raku` keeps the
            // instance repr — its `WHEREFORE` is the documented routine, whose
            // own `.raku` embeds a per-run object address.
            "Pod::Block::Declarator" if method == "gist" => {
                return Some(Ok(Value::str(target.to_string_value())));
            }
            cls @ ("ThreadPoolScheduler" | "CurrentThreadScheduler") => {
                return Some(Ok(Value::str(
                    crate::builtins::methods_0arg::raku_repr::scheduler_raku_repr(cls),
                )));
            }
            "Supplier" => {
                return Some(Ok(Value::str(
                    "Supplier.new(taplist => Supplier::TapList.new)".to_string(),
                )));
            }
            // Rakudo's `Collation` holds one `$!collation-level` attribute,
            // which its default `.raku` lists; mutsu stores the four levels.
            "Collation" => {
                let settings = crate::builtins::collation::CollationSettings::from_value(target);
                return Some(Ok(Value::str(format!(
                    "Collation.new(collation-level => {})",
                    settings.collation_level()
                ))));
            }
            "IO::Path::Parts" => {
                let attr_map = attributes.as_map();
                let rendered: Vec<String> = ["volume", "dirname", "basename"]
                    .iter()
                    .map(|k| {
                        let v = attr_map
                            .get(*k)
                            .cloned()
                            .unwrap_or_else(|| Value::str_from(""));
                        self.call_method_with_values(v.clone(), "raku", vec![])
                            .map(|r| r.to_string_value())
                            .unwrap_or_else(|_| v.to_string_value())
                    })
                    .collect();
                return Some(Ok(Value::str(format!(
                    "IO::Path::Parts.new({})",
                    rendered.join(",")
                ))));
            }
            _ if is_stash_class_name(class_name.as_str())
                && (method == "raku") =>
            {
                let symbols = attributes
                    .as_map()
                    .get("symbols")
                    .cloned()
                    .unwrap_or_else(|| Value::hash(ValueMap::default()));
                return Some(Ok(Value::str(
                    crate::builtins::methods_0arg::raku_repr::raku_value(&symbols),
                )));
            }
            // Rakudo's `Backtrace::Frame` has no custom `.gist`, so it falls
            // back to the default `Backtrace::Frame.new(file => ..., line =>
            // ..., code => ..., subname => ...)` rendering for BOTH `.gist`
            // and `.raku` -- unlike the plain `.Str` text (`  in block ... at
            // ... line N`), which `Str`/`gist`'s native fast path already
            // handles and this arm never sees. `code` has no attribute of its
            // own (it is synthesized on demand by the frame's `code`
            // accessor), so it is rebuilt here the same way and rendered
            // through the ordinary Code `.raku` dispatch.
            // `HyperSeq.configuration` has no class declaration to read its
            // public attributes from: its two fields are the whole object.
            "HyperConfiguration" if method == "raku" => {
                let attrs = attributes.as_map();
                let fields: Vec<String> = ["batch", "degree"]
                    .iter()
                    .filter_map(|k| {
                        attrs.get(*k).map(|a| {
                            let a = crate::builtins::methods_0arg::raku_repr::raku_value(a);
                            format!("{k} => {a}")
                        })
                    })
                    .collect();
                return Some(Ok(Value::str(format!(
                    "HyperConfiguration.new({})",
                    fields.join(", ")
                ))));
            }
            "Backtrace::Frame" => {
                let attr_map = attributes.as_map();
                let file_value = attr_map
                    .get("file")
                    .cloned()
                    .unwrap_or_else(|| Value::str(String::new()));
                let line_value = attr_map.get("line").cloned().unwrap_or(Value::int(0));
                let subname_value = attr_map
                    .get("subname")
                    .cloned()
                    .unwrap_or_else(|| Value::str(String::new()));
                let code_value = crate::builtins::backtrace_methods::frame_code_value(&attributes);
                let raku_of = |interp: &mut Interpreter, v: Value| -> String {
                    interp
                        .call_method_with_values(v.clone(), "raku", vec![])
                        .map(|r| r.to_string_value())
                        .unwrap_or_else(|_| v.to_string_value())
                };
                let file = raku_of(self, file_value);
                let line = raku_of(self, line_value);
                let code = raku_of(self, code_value);
                let subname = raku_of(self, subname_value);
                return Some(Ok(Value::str(format!(
                    "Backtrace::Frame.new(file => {file}, line => {line}, code => {code}, subname => {subname})"
                ))));
            }
            _ => {}
        }
        let class_key = class_name.resolve();
        let display_name = crate::value::user_facing_type_name(&class_key);
        // A CStruct's fields live in C memory (a body it owns, or the memory C
        // handed back): read them out before rendering them.
        self.seed_cstruct_fields_for_method(&class_key, Some(target));
        self.raku_cycle_guards.leaf.enter(target_id);
        let public_attrs = self.collect_public_raku_attrs(&class_key, &(attributes).as_map());
        let cycle_hit = self.raku_cycle_guards.leaf.leave(&target_id);
        let body = if public_attrs.is_empty() {
            format!("{}.new", display_name)
        } else {
            format!("{}.new({})", display_name, public_attrs.join(", "))
        };
        if cycle_hit {
            return Some(Ok(Value::str(format!(
                "(my \\{} = {})",
                Self::raku_leaf_backref_name(&class_key, target_id),
                body
            ))));
        }
        Some(Ok(Value::str(body)))
    }

    /// Read a public auto-generated attribute accessor without entering method
    /// dispatch again. This terminal is used by a wrapped accessor's
    /// `callsame` frame; the normal instance fallback below has additional
    /// built-in cases, but ordinary declared accessors share this core.
    pub(crate) fn read_public_attribute_accessor(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !args.is_empty() {
            return None;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let class_name = class_name.resolve();
        let attr = self
            .collect_class_attributes(&class_name)
            .into_iter()
            .find(|attr| attr.is_public && attr.name == method)?;
        let value = attributes
            .as_map()
            .get(method)
            .cloned()
            .unwrap_or(Value::NIL);
        if let Some(msg) = self.class_attribute_deprecated(&class_name, method) {
            self.check_deprecation_for_method(method, &class_name, &msg);
        }
        if matches!(attr.sigil, '@' | '%')
            && !value.is_nil()
            && self.container_type_metadata(&value).is_none()
            && let Some(type_constraint) = self.get_attr_type_constraint(&class_name, method)
        {
            let (value_type, key_type) =
                crate::runtime::types::split_object_hash_constraint(&type_constraint);
            let info = crate::runtime::ContainerTypeInfo {
                value_type: value_type.to_string(),
                key_type: key_type.map(str::to_string),
                declared_type: None,
            };
            return Some(Ok(self.tag_container_metadata(value, info)));
        }
        Some(Ok(value))
    }

    /// Dispatch an auto-generated accessor through its method-wrap chain.
    /// `callsame` in the wrapper advances to the terminal accessor entry that
    /// `dispatch_next_candidate` consumes without re-entering this chain.
    ///
    /// `terminal` is the chain's final `DeferralEntry::Accessor`; its
    /// `want_container` asks for the attribute's container rather than its
    /// value.
    /// Whether a `.wrap` wrapper hands its result back as a container (`is
    /// rw` / `is raw`). A `Method` object found with `.^find_method` (a role's
    /// `method w($self: |c) is rw`, Staticish's wrappers) carries the callable
    /// that runs it; its traits are that callable's.
    // Cost: O(1).
    fn wrapper_declares_container_return(wrapper: &Value) -> bool {
        match wrapper.view() {
            ValueView::Sub(data) => data.declares_container_return(),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if matches!(class_name.as_str(), "Method" | "Submethod") => attributes
                .as_map()
                .get("__mutsu_method_callable")
                .is_some_and(|callable| {
                    matches!(callable.view(), ValueView::Sub(data) if data.declares_container_return())
                }),
            _ => false,
        }
    }

    pub(crate) fn dispatch_wrapped_attribute_accessor(
        &mut self,
        target: Value,
        receiver_class: &str,
        args: Vec<Value>,
        chain: Vec<(u64, Value)>,
        mut terminal: super::DeferralEntry,
    ) -> Result<Value, RuntimeError> {
        let super::DeferralEntry::Accessor {
            name: method,
            want_container,
            ..
        } = &mut terminal
        else {
            unreachable!("a wrapped accessor chain ends in its accessor entry");
        };
        let method = method.clone();
        let method = method.as_str();
        // Every wrapper sits between the terminal and the caller, and a plain
        // (non-rw) one decontainerizes its return in raku — `B.v = 5` through
        // `method ($s: |c) { callsame }` is `X::Assignment::RO`. So the
        // container request reaches the terminal only when the whole chain
        // hands its result back as a container.
        *want_container = *want_container
            && chain
                .iter()
                .all(|(_, w)| Self::wrapper_declares_container_return(w));
        self.push_method_samewith_context(receiver_class, method, &args, Some(target.clone()));
        self.push_wrapped_accessor_dispatch_frame(
            receiver_class,
            &args,
            target.clone(),
            &chain,
            terminal,
        );
        let outermost = chain.last().expect("accessor wrap chain is non-empty");
        let mut call_args = vec![target];
        call_args.extend(args);
        self.shift_arg_sources_for_wrap_invocant();
        let result = self.call_sub_value(outermost.1.clone(), call_args);
        self.pop_method_samewith_context();
        self.dispatch.method_dispatch_stack.pop();
        result
    }

    /// Dispatch method calls for Instance values and handle all fallback paths.
    ///
    /// This is called from `call_method_with_values` after the main method name
    /// match and enum/promise/channel/mixin dispatch.
    pub(super) fn dispatch_instance_and_fallback(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Instance dispatch
        if let ValueView::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target.view()
        {
            // `.^lookup`/`.^find_method` return Method/Submethod instances,
            // whose callable payload already carries the invocant as its
            // first parameter. Reuse the callable's normal `.assuming`
            // implementation so a looked-up method can be curried before it
            // is called (`$type.^lookup('method').assuming($obj)(...)`).
            if method == "message"
                && args.is_empty()
                && let Some(msg) = attributes.as_map().get("__mutsu_thrown_message")
            {
                return Ok(msg.clone());
            }
            // A per-attribute container descriptor produced by `Attribute.container`
            // (tagged with `__mutsu_attr_container_owner`). `.VAR` returns the
            // descriptor itself so a subsequent `does Role(...)` can be recorded
            // against the owning attribute (see `exec_does_op`); every other method
            // delegates to a plain empty container of the attribute's sigil so
            // introspection (`.container.shape`, ...) keeps working.
            if attributes
                .as_map()
                .contains_key("__mutsu_attr_container_owner")
            {
                if method == "VAR" {
                    return Ok(target.clone());
                }
                let sigil = attributes
                    .as_map()
                    .get("__mutsu_attr_container_sigil")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "$".to_string());
                let base = match sigil.as_str() {
                    // An `@` attribute's container is an `Array`, as in Rakudo,
                    // not a `List` (`.container.shape` is `Array`'s).
                    "@" => Value::real_array(Vec::new()),
                    "%" => Value::hash(ValueMap::default()),
                    _ => Value::NIL,
                };
                return self.call_method_with_values(base, method, args);
            }
            // `Duration` does `Real`: delegate numeric methods (e.g.
            // `(now - now).abs`) to its underlying numeric value. `.Str` / `.gist`
            // / `.raku` and other identity methods keep Duration's own behaviour.
            if class_name.resolve() == "Duration"
                && matches!(
                    method,
                    "abs"
                        | "floor"
                        | "ceiling"
                        | "round"
                        | "truncate"
                        | "sign"
                        | "Int"
                        | "Rat"
                        | "FatRat"
                        | "Real"
                        | "sqrt"
                )
            {
                let num = crate::runtime::utils::coerce_to_numeric(target.clone());
                return self.call_method_with_values(num, method, args);
            }
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self.private_calling_package();
                // An unqualified external private call (`$o!meth` where `$o` is
                // not `self`) must name the defining package — Raku reports
                // X::Method::Private::Unqualified rather than the Permission
                // error used for a qualified-but-untrusted call.
                let was_qualified = crate::qualified::is_qualified_str(private_rest);
                // Resolve: owner-qualified (!Owner::method) or unqualified (!method).
                // Split at the LAST `::` — the owner class may itself be a nested
                // name (`$c!Jar::Cookie::secret` is owner `Jar::Cookie`, method
                // `secret`, NOT owner `Jar`, method `Cookie::secret`).
                let (pm_name, owner_only, resolved) = if let Some((owner_class, pm_name)) =
                    crate::qualified::split_qualified(crate::qualified::known_symbol(private_rest))
                        .map(|(head, tail)| (head.as_str(), tail.as_str()))
                {
                    // `owner_class` is the short name as written in
                    // source; canonicalize it relative to the caller's
                    // package chain before using it both for the trust
                    // check and for the actual MRO lookup below — an
                    // uncanonicalized short name never matches a fully
                    // qualified MRO entry (`"A" != "M::A"`).
                    let invocant_class = class_name.resolve();
                    let (canonical_owner, caller_allowed) = self
                        .resolve_and_check_private_owner_on(
                            caller_class.as_deref(),
                            owner_class,
                            Some(&invocant_class),
                        );
                    // A qualified private call is resolved LEXICALLY, like
                    // the unqualified one below: `method_class_stack` names
                    // whoever *invoked* the running code, which is the wrong
                    // class when the call sits in a closure that some other
                    // object calls back into (`$holder.run: { $self!R::p }`).
                    // Fall back to the captured `self` the same way, instead
                    // of denying the call outright.
                    if !caller_allowed && !self.lexical_self_is_private_owner(&canonical_owner) {
                        return Err(make_private_permission_error(
                            pm_name,
                            &canonical_owner,
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    (
                        pm_name,
                        Some(canonical_owner.clone()),
                        self.resolve_private_method_with_owner(
                            &class_name.resolve(),
                            &canonical_owner,
                            pm_name,
                            &args,
                        ),
                    )
                } else {
                    (
                        private_rest,
                        None,
                        self.resolve_private_method_any_owner(
                            &class_name.resolve(),
                            private_rest,
                            &args,
                        ),
                    )
                };
                // Nothing matched by signature. That is not the same as "no such
                // private method": the class may declare exactly this method with
                // parameters these arguments fail to bind, and raku reports the
                // binding failure (`Type check failed in binding to parameter
                // '$n'`, `Too many positionals passed`) — as mutsu's own PUBLIC
                // dispatch already does. Re-resolve by name so the call runs and
                // the binding machinery produces the real error; several
                // candidates are a genuine X::Multi::NoMatch. Only when there is
                // no candidate at all does the not-found error below stand.
                let resolved = match resolved {
                    Some(r) => Some(r),
                    None => {
                        let mut candidates = self.private_method_candidates_by_name(
                            &class_name.resolve(),
                            owner_only.as_deref(),
                            pm_name,
                        );
                        match candidates.len() {
                            0 => None,
                            1 => Some(candidates.remove(0)),
                            _ => return Err(make_multi_no_match_error(pm_name)),
                        }
                    }
                };
                if let Some((resolved_owner, method_def)) = resolved {
                    // Code written in a role reaches the private methods the
                    // role composed into its consumer: an unqualified
                    // `$obj!m` there resolves against the role, not a caller.
                    let caller_allowed = self
                        .private_owner_trusts_caller(caller_class.as_deref(), &resolved_owner)
                        || self.lexical_self_allows_private(&resolved_owner)
                        || (!was_qualified
                            && caller_class.as_deref().is_some_and(|caller| {
                                self.registry().roles.contains_key(caller)
                                    && self.class_does_role(&resolved_owner, caller)
                            }));
                    if !caller_allowed {
                        if was_qualified {
                            return Err(make_private_permission_error(
                                pm_name,
                                &class_name.resolve(),
                                caller_class.as_deref().unwrap_or("GLOBAL"),
                            ));
                        }
                        return Err(make_private_unqualified_error(pm_name));
                    }
                    let (result, updated) = self.run_resolved_method_compiled_or_treewalk(
                        &class_name.resolve(),
                        &resolved_owner,
                        pm_name,
                        method_def,
                        attributes.to_map(),
                        args,
                        Some(target.clone()),
                    )?;
                    attributes.commit_attrs(updated);
                    return Ok(result);
                }
                // Private method not found — fall back to private attribute access.
                // In Raku, `$obj!Owner::attr` accesses the private attribute `$!attr`
                // directly when no explicit private method is defined.
                if args.is_empty()
                    && let Some(val) = attributes.as_map().get(pm_name)
                {
                    return Ok(val.clone());
                }
                return Err(make_method_not_found_error(
                    pm_name,
                    &class_name.resolve(),
                    true,
                ));
            }

            // IO::Spec methods — delegate to the Package-based handler (the
            // path methods are class methods). Repr methods stay on the
            // instance: `IO::Spec::Unix.new.gist` is `IO::Spec::Unix.new`
            // (the generic instance repr), not the type object's `(Unix)`.
            if (class_name == "IO::Spec" || class_name.as_str().starts_with("IO::Spec::"))
                && !matches!(method, "gist" | "raku" | "Str" | "Stringy")
            {
                let pkg = Value::package(class_name);
                return self.call_method_with_values(pkg, method, args);
            }
            // The compile-time resolver `$*R` and its attach targets
            // (`runtime::attach_target`).
            if matches!(
                class_name.resolve().as_str(),
                crate::runtime::attach_target::RESOLVER_CLASS
                    | crate::runtime::attach_target::ATTACH_TARGET_CLASS
            ) && let Some(result) =
                self.dispatch_attach_target_method(&class_name.resolve(), &target, method, &args)
            {
                return result;
            }
            // The `Block` a variable trait handler reaches through `Variable.block`.
            if matches!(
                class_name.resolve().as_str(),
                "Block" | "Sub" | "Method" | "Submethod"
            ) && attributes.as_map().contains_key(crate::runtime::attach_target::VARIABLE_BLOCK_MARK)
                && let Some(result) = self.dispatch_variable_block_method(method, &args)
            {
                return result;
            }
            // The compile-time $*LANG object graph (ADR-0026 slang activation):
            // slang_grammar/slang_actions/define_slang on Mutsu::Slang::CompLang.
            if class_name.as_str().starts_with("Mutsu::Slang::")
                && let Some(result) =
                    self.dispatch_slang_comp_lang_method(&class_name.resolve(), method, &args)
            {
                return result;
            }
            // A user ClassHOW subclass inherits the metamodel's `can`, whose
            // first argument is the type being introspected. Let it reach the
            // ClassHOW fallback below; treating that argument as an ordinary
            // instance `.can` method name loses the requested MRO lookup
            // (`Foo.^can('hello')`, #13919).
            if method == "can" && !self.is_metamodel_how_class(&class_name.resolve()) {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if method_name.is_empty() {
                    return Ok(Value::array(Vec::new()));
                }
                let results = self.collect_can_methods(&target, &method_name);
                return Ok(Value::array(results));
            }
            // `Proc::Async`'s, `Supply`'s, `IO::CatHandle`'s, `Supplier`'s, the
            // schedulers' and the handle classes' methods are rows, found by their
            // owner; they commit through the instance's shared cell (ADR-11276
            // §9.128, §9.130, §9.132, §9.133, §9.157).
            // A method the subclass declares itself wins over the row.
            if let Some(owners) = self.carried_state_owners_of(&target)
                && !self.has_user_method(&class_name.resolve(), method)
                && let Some(result) = crate::builtins::method_table::invoke_owner(
                    self,
                    owners,
                    method,
                    &args,
                    || target.clone(),
                )
            {
                return result;
            }
            // A user-defined method shadows an inherited native one: for a user
            // subclass of a builtin (`class S is IO::Handle { method Str {...} }`),
            // `is_native_method` finds the base class's native `Str` via the MRO,
            // but the subclass's own method is more derived and must win — it is
            // dispatched by the `has_user_method` block further down. Without
            // this gate, `~$fh` on a Text::IO::String stringified through the
            // native IO::Handle `Str` ("IO::Handle()") instead of the user
            // `method Str` (Text::CSV 46_eol_si).
            if self.is_native_method(&class_name.resolve(), method)
                && !self.grammar_has_user_method(&class_name.resolve(), method)
            {
                let cls = class_name.resolve();
                if Self::native_method_blocks_on_other_thread(&cls, method) {
                    let snapshot = attributes.to_map();
                    return self.call_native_instance_method(&cls, &snapshot, method, args);
                }
                return self.call_native_instance_method(
                    &cls,
                    &(attributes).as_map(),
                    method,
                    args,
                );
            }
            if method == "isa" {
                let isa_arg = args.first().cloned().unwrap_or(Value::NIL);
                let target_name = match isa_arg.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    // `isa(Mu \type)`: any other value stands for its own type.
                    _ => crate::value::types::what_type_name(&isa_arg),
                };
                // isa only matches classes, not roles. If the target is a role
                // that has NOT been punned to a class, return False (use .does
                // for role checking).
                if self.registry().roles.contains_key(&target_name)
                    && !self.registry().classes.contains_key(&target_name)
                {
                    return Ok(Value::FALSE);
                }
                if self
                    .class_mro(&class_name.resolve())
                    .contains(&crate::symbol::Symbol::intern(&target_name))
                {
                    return Ok(Value::TRUE);
                }
                // A grammar's parse cursor is a `Match` by SHAPE, whatever its
                // grammar's registration looks like (raku: `Grammar` IS a
                // `Match` subclass, so a cursor reports the grammar's own class
                // name). The registry MRO gets there for a grammar declared as
                // a statement, but an anonymous one (`my grammar { … }`) is a
                // bare package with no parents — `isa_check` answers
                // `Match`/`Capture`/`Cool` from the value's shape.
                return Ok(Value::truth(
                    target.is_match_instance() && target.isa_check(&target_name),
                ));
            }
            // `X::AdHoc.payload` returns the value passed to `die` (mutsu stores
            // it under `payload`). An X::AdHoc synthesized from an internal
            // `RuntimeError` — a native dlopen failure, a parse error inside
            // EVAL, … — carries only a `message` attr (no `payload`), so fall
            // back to that string so `.payload.starts-with(...)` / `.payload.Str`
            // keep working (raku: `X::AdHoc` made from `die "msg"` has
            // `.payload eq "msg"`).
            // An Exception instance stringifies (.Str/.gist/~) and reports via its
            // `.message` (raku: Exception.Str and .gist both return .message). Detect
            // it via the MRO so a user `class E is Exception` (whose name is not
            // `X::*`/`*Exception`) is covered too. A user-defined `gist`/`Str`/
            // `message` already won dispatch above, so reaching here is the default.
            if args.is_empty() && matches!(method, "gist" | "Str" | "Stringy" | "message") {
                let cn = class_name.resolve();
                let is_exception = !target.is_match_instance()
                    && (target.instance_is_exception_by_name()
                        || cn.ends_with("Exception")
                        || self
                            .class_mro(&cn)
                            .iter()
                            .any(|p| p == "Exception" || p == "Failure"));
                if is_exception {
                    if method == "message"
                        && let Some(msg) = attributes.as_map().get("__mutsu_thrown_message")
                    {
                        return Ok(msg.clone());
                    }
                    // This arm is only the DEFAULT implementation of
                    // `Exception.message`/`.gist`/`.Str`. A class that defines
                    // the method itself must win, so fall through to the normal
                    // user-method resolution further down instead of answering
                    // from the stored attribute. (Until 2026-07-25 the guard read
                    // `method != "message"`, which made a user `method message`
                    // unreachable through this path: an exception that computes
                    // its message reported the literal text `(Any)`.)
                    // raku's `Exception.gist` is the message *plus the
                    // backtrace*; `.Str`/`.message` are the bare message. The
                    // native `X::*` arm in `methods_0arg` already appends it —
                    // this is the same rule for the classes only the
                    // interpreter can recognise as exceptions (a user
                    // `class E is Exception`).
                    let with_backtrace = |target: &Value, msg: String| -> Value {
                        if method != "gist" {
                            return Value::str(msg);
                        }
                        match Self::exception_backtrace_text(target) {
                            Some(bt) => Value::str(format!("{}\n{}", msg, bt)),
                            None => Value::str(msg),
                        }
                    };
                    if self.has_user_method(&cn, method) {
                        // fall through to user-method dispatch
                    } else if method != "message" && self.has_user_method(&cn, "message") {
                        // For .Str/.gist, prefer a user-defined `message` method
                        // (it may interpolate attributes).
                        let msg =
                            self.call_method_with_values(target.clone(), "message", vec![])?;
                        return Ok(with_backtrace(&target, msg.to_string_value()));
                    } else if let Some(msg) = attributes.as_map().get("message")
                        // A declared-but-undefined `has $.message` is not a
                        // message; rendering it would print the literal `(Any)`.
                        // `.message` itself falls through to the accessor (raku
                        // returns the undefined value), `.gist`/`.Str` take the
                        // no-message rendering below.
                        && !msg.is_nil()
                        && !matches!(msg.view(), ValueView::Package(_))
                    {
                        let msg = msg.to_string_value();
                        return Ok(with_backtrace(&target, msg));
                    } else if method == "message" {
                        // A class that reaches Exception through a role still
                        // has Exception.message even when it has no stored
                        // message attribute. Match the native row's empty
                        // default instead of falling through to "No such
                        // method" for user-named exception classes.
                        return Ok(Value::str(String::new()));
                    } else if cn == "X::AdHoc"
                        // `X::AdHoc` (what `die "..."` builds) carries its text in
                        // `payload`, not `message`.
                        && let Some(payload) = attributes.as_map().get("payload")
                        && !payload.to_string_value().is_empty()
                    {
                        let msg = payload.to_string_value();
                        return Ok(with_backtrace(&target, msg));
                    } else if let Some(formatted) =
                        crate::value::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Ok(with_backtrace(&target, formatted));
                    } else if method != "message" {
                        // raku's `Exception.gist` for an exception with nothing
                        // to say: it names the class instead of stringifying the
                        // undefined `message` attribute. `.throw` stamps a
                        // `backtrace` attribute, which is what distinguishes a
                        // thrown exception from one that was merely constructed.
                        // (`X::AdHoc`'s payload and the class-specific formatted
                        // messages were already tried above.)
                        let thrown = attributes.as_map().contains_key("backtrace");
                        let msg = if thrown {
                            format!("Died with {}", cn)
                        } else {
                            format!("Unthrown {} with no message", cn)
                        };
                        return Ok(with_backtrace(&target, msg));
                    }
                }
            }
            // `Exception.backtrace` for a class only the MRO identifies as an
            // exception (`class E is Exception`): the native `X::*` arm in
            // `methods_0arg` is name-gated and never sees it, so `$!.backtrace`
            // used to die with "No such method". `.throw` stamps the attribute;
            // an exception that was merely constructed answers `Nil`, as raku's
            // does.
            if args.is_empty()
                && method == "backtrace"
                && !self.has_user_method(&class_name.resolve(), "backtrace")
                && self.value_is_exception_instance(&target)
            {
                return Ok(attributes
                    .as_map()
                    .get("backtrace")
                    .cloned()
                    .unwrap_or(Value::NIL));
            }
            // Default `.gist` of a user instance matches its `.raku` (raku:
            // `say F.new(:z(5))` → `F.new(z => 5)`), unless the class defines its
            // own `gist`. Without this, a defined instance gisted as `F()` (the
            // type-object form), e.g. `say $obj`, `$obj.=meth` round-trips.
            // See `default_instance_repr` for the rendering itself (shared with
            // `native_any_base_next_candidate`'s callsame/nextsame fallback).
            if !self.has_user_method(&class_name.resolve(), method)
                && let Some(result) = self.default_instance_repr(&target, method, &args)
            {
                return result;
            }
            // A Version subclass stores its native payload separately because
            // ValueView::Instance carries the subclass name. Delegate the
            // payload-only accessors that are not representation methods after
            // user methods have had a chance to win.
            if matches!(method, "parts" | "plus" | "whatever")
                && !self.has_user_method(&class_name.resolve(), method)
                && let Some(payload) = attributes.as_map().get("__mutsu_version_value")
            {
                return self.call_method_with_values(payload.clone(), method, args);
            }
            if method == "clone"
                && let Some(result) = self.native_instance_clone_value(&target, &args)
            {
                return result;
            }
            // User-defined methods take priority over auto-generated accessors
            // only when they win the per-MRO-level race: a child class's
            // accessor shadows a parent's explicit method, and a method that
            // comes ONLY from a composed role does not shadow the class's own
            // attribute accessor at the same level (class entities are
            // prioritized over role entities). When the accessor wins, fall
            // through to the accessor block below.
            let cn_resolved = class_name.resolve();
            let accessor_wins = matches!(
                self.resolve_user_method_or_accessor(&cn_resolved, method),
                Some(UserMethodOrAccessor::Accessor)
            );
            if !accessor_wins && self.grammar_has_user_method(&cn_resolved, method) {
                // ADR-0019 F6: try the VM-level direct compiled-dispatch path
                // first (no `run_instance_method` carrier, no recursion risk —
                // see `try_dispatch_compiled_method_direct`'s doc comment).
                // `target` and `attributes` share the same underlying cell
                // (ADR-0013), and `dispatch_compiled_method` already commits
                // any reconciled attribute map back through that cell, so a
                // fresh `attributes.to_map()` read after the call reflects the
                // post-mutation state without needing the carrier's own
                // returned snapshot (same reasoning as the mut-lvalue family's
                // migration).
                if let Some(result) =
                    self.try_dispatch_compiled_method_direct(&target, method, &args)
                {
                    let result = result?;
                    let updated = attributes.to_map();
                    if result.is_proxy_value()
                        && self.should_fetch_returned_proxy(&class_name.resolve(), method)
                        && matches!(result.view(), ValueView::Proxy { .. })
                    {
                        return self.proxy_fetch(
                            &result,
                            None,
                            &class_name.resolve(),
                            &updated,
                            target_id,
                        );
                    }
                    return Ok(result);
                }
                let (result, updated) = self.run_instance_method_at(
                    "instanceops",
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                attributes.commit_attrs(updated.clone());
                // Auto-FETCH if the method returned a Proxy
                if result.is_proxy_value()
                    && self.should_fetch_returned_proxy(&class_name.resolve(), method)
                    && matches!(result.view(), ValueView::Proxy { .. })
                {
                    return self.proxy_fetch(
                        &result,
                        None,
                        &class_name.resolve(),
                        &updated,
                        target_id,
                    );
                }
                return Ok(result);
            }
            // Fallback: auto-generated accessor for public attributes.
            if args.is_empty() {
                if let Some(owner) = self.attribute_accessor_owner(&cn_resolved, method)
                    && let Some(chain) =
                        self.get_method_wrap_chain(owner.resolve().as_str(), method, 0)
                {
                    return self.dispatch_wrapped_attribute_accessor(
                        target,
                        &cn_resolved,
                        args,
                        chain,
                        super::DeferralEntry::Accessor {
                            owner,
                            name: method.to_string(),
                            want_container: false,
                        },
                    );
                }
                // An `is repr('CStruct')` handle stores no Raku attributes: its
                // fields live in the C struct the instance's `address` points
                // at, so the accessor reads them out of native memory
                // (`$ssl.server`, `nativecast(evp_cipher_st, $c).key_len`).
                if let Some(field) = self.cstruct_field_value(&target, method) {
                    return Ok(field);
                }
                // `$ptr.of` / `$ptr.deref` on a NativeCall `Pointer`. The prelude
                // class declares neither, so they arrive here.
                if let Some(result) = self.try_pointer_method(&target, method) {
                    return result;
                }
                let cn = class_name.resolve();
                // A `Method`/`Regex` Instance from `.^lookup`/`.^find_method`
                // (built by `make_native_method_object_ex`/
                // `make_method_object_with_owner_ex`) is a `Routine` in real
                // Rakudo and answers `.arity`/`.count` like any other, but
                // neither name is a stored attribute the generic accessor
                // fallback below can serve. Both numbers are derivable from
                // the `signature` attribute this Instance already carries
                // (#8416), so read the `SigInfo` back off the materialized
                // `Signature` rather than recomputing it from scratch --
                // that keeps them from ever drifting apart.
                if matches!(cn.as_str(), "Method" | "Regex")
                    && matches!(method, "arity" | "count")
                    && let Some(sig_val) = attributes.as_map().get("signature")
                    && let Some(info) = crate::value::signature::extract_sig_info(sig_val)
                {
                    return Ok(if method == "arity" {
                        Value::int(Self::signature_required_positional_count(&info))
                    } else {
                        Self::signature_count_value(&info)
                    });
                }
                let class_attrs = self.collect_class_attributes(&cn);
                // A grammar *cursor* -- the invocant Raku hands to `FAILGOAL`
                // and friends -- is an instance of the grammar itself, and
                // every grammar inherits Cursor's positional state. mutsu mints
                // such a cursor with the state in its attribute map, but the
                // grammar declares no `has $.pos`, so the user-declared-class
                // rule below would reject `self.pos`. Answer the Cursor
                // attributes from the stored map instead. A grammar that
                // declares a method or an attribute of the same name still
                // wins: the method lookup already failed above, and a declared
                // attribute is excluded here.
                if Self::is_cursor_state_attribute(method)
                    && !class_attrs.iter().any(|attr| attr.name == method)
                    && let Some(val) = attributes.as_map().get(method)
                    && self.mro_readonly(&cn).iter().any(|c| c == "Grammar")
                {
                    return Ok(val.clone());
                }
                // For a *user-declared* class the collected public-attribute list is
                // authoritative: a `.name` accessor resolves ONLY for a declared
                // public `has $.name`. An undeclared name (e.g. an unknown named arg
                // `.new` accepted and stored) is NOT an accessor and falls through to
                // X::Method::NotFound (Rakudo: `class C {}; C.new(x=>3).x` dies). A
                // *built-in* class (exception types, ...) keeps its attributes only
                // in the stored map (not collected), so still read them.
                // `IO::Path.path` is the stored path text; a user subclass of
                // `IO::Path` inherits it even though its own declared
                // attributes (or none) are all the collected list holds.
                if method == "path"
                    && args.is_empty()
                    && !class_attrs.iter().any(|attr| attr.name == method)
                    && let Some(val) = attributes.as_map().get(method)
                    && self.mro_readonly(&cn).iter().any(|c| c == "IO::Path")
                {
                    return Ok(val.clone());
                }
                if class_attrs.is_empty() {
                    if !self.types.user_declared_classes.contains(&cn)
                        && let Some(val) = attributes.as_map().get(method)
                    {
                        // Check for deprecated attribute accessor
                        if let Some(msg) = self.class_attribute_deprecated(&cn, method) {
                            self.check_deprecation_for_method(method, &cn, &msg);
                        }
                        return Ok(val.clone());
                    }
                } else {
                    for attr in &class_attrs {
                        if attr.is_public && attr.name == method {
                            // Check for deprecated attribute accessor
                            if let Some(msg) = self.class_attribute_deprecated(&cn, method) {
                                self.check_deprecation_for_method(method, &cn, &msg);
                            }
                            let stored = attributes.as_map().get_vivify(method).cloned();
                            let val = match stored {
                                Some(val) => val,
                                // A grammar cursor is minted without BUILD
                                // (raku: `nqp::create`), so a declared attribute
                                // no rule wrote reads as its uninitialised value
                                // -- the type object, an empty `@` / `%` -- not
                                // as `Nil`, and a declared `= default` is not
                                // applied (#9803).
                                None if attributes.contains_key(
                                    crate::value::match_view::cursor_match_marker(),
                                ) =>
                                {
                                    let constraints = self.collect_attribute_type_constraints(&cn);
                                    self.seed_attr_value(&cn, method, attr.sigil, &constraints)
                                }
                                None => Value::NIL,
                            };
                            // The generated accessor decontainerizes: `self.a`
                            // is not itemized even when the store itemized the
                            // attribute's Scalar (`$!a` / `$.a` preserve it) --
                            // UNLESS the accessor is `is rw`, which hands back
                            // the writable Scalar itself, item-ness included.
                            let val = if attr.is_rw {
                                val
                            } else {
                                Self::accessor_read_value(attr.sigil, val)
                            };
                            // A non-rw accessor hands back a decontainerized value, so
                            // an attribute bound to a Proxy (AttrX::Mooish's lazy
                            // attribute) is read through it: the FETCH runs at the call,
                            // even in statement position, where nothing else would.
                            let val = if !attr.is_rw {
                                let inner = val.deref_container();
                                if inner.is_proxy_value() {
                                    self.auto_fetch_proxy(&inner)?
                                } else {
                                    val
                                }
                            } else {
                                val
                            };
                            // For typed @/% attributes, register type metadata on the
                            // returned value so push/insert type enforcement works.
                            if (attr.sigil == '@' || attr.sigil == '%')
                                && !val.is_nil()
                                && self.container_type_metadata(&val).is_none()
                                && let Some(tc) =
                                    self.get_attr_type_constraint(&cn, method.as_ref())
                            {
                                let info = crate::runtime::ContainerTypeInfo {
                                    value_type: tc.clone(),
                                    key_type: None,
                                    declared_type: Some(tc),
                                };
                                return Ok(self.tag_container_metadata(val, info));
                            }
                            return Ok(val);
                        }
                    }
                    // A synthesized mixin type over a built-in base
                    // (`$method does R` reblesses into `Method+{R}`, see
                    // `ensure_mixin_class`) is not user-declared: its collected
                    // list holds only the roles' attributes, while the base's
                    // own attributes (`.name`, `.rw`, `.package`, ...) still
                    // live in the stored map, exactly as for the bare base.
                    if !self.types.user_declared_classes.contains(&cn)
                        && !class_attrs.iter().any(|attr| attr.name == method)
                        && let Some(val) = attributes.as_map().get(method)
                    {
                        return Ok(val.clone());
                    }
                }
            }
            // Enum-as-role dispatch: if the class `does` an enum, check variant methods
            if args.is_empty()
                && let Some(enum_names) = self
                    .registry()
                    .class_enum_roles
                    .get(&class_name.resolve())
                    .cloned()
            {
                for enum_name in &enum_names {
                    if let Some(variants) = self.registry().enum_types.get(enum_name).cloned()
                        && variants.iter().any(|(vname, _)| vname == method)
                    {
                        // Get the stored enum value from the instance attribute
                        let map = attributes.as_map();
                        let stored = map.get(enum_name);
                        if let Some(ValueView::Enum { key, .. }) = stored.map(Value::view) {
                            return Ok(Value::truth(key.resolve() == method));
                        }
                        // No enum value set
                        return Ok(Value::FALSE);
                    }
                }
            }
        }

        // `Cool`'s numeric methods are `self.Numeric.METHOD` (Rakudo's
        // `Cool.abs` and kin), so an instance of a `Cool` subclass computes
        // them through its own `Numeric` (`class C is Cool { method Numeric
        // {...} }; C.new.abs`, `roast/S32-num/cool-num.t`). A `Real` class
        // has the role's rows instead (ADR-11276 §9.179).
        // TODO: compile to bytecode -- a `Cool` row reached through the
        // instance's MRO walk (`shapeless_row`) with an interpreter twin that
        // calls the instance's `Numeric`.
        if let ValueView::Instance { class_name, .. } = target.view()
            && crate::value::cool_numeric_method(method)
            && self.has_user_method(class_name.as_str(), "Numeric")
            && self
                .class_mro(class_name.as_str())
                .iter()
                .any(|c| c.as_str() == "Cool")
            && let Ok(numeric) = self.call_method_with_values(target.clone(), "Numeric", vec![])
            && numeric != target
        {
            return self.call_method_with_values(numeric, method, args);
        }

        // A subclass of a built-in type (`class P is Pair`, `class B is Bag`,
        // `class I is Int`) inherits every method of its base: a method the
        // class does not declare runs on the base value it carries
        // (`native_base::redirect`), as the table's guard step answers it.
        if let ValueView::Instance { class_name, .. } = target.view()
            && let Some(payload) = crate::builtins::native_base::redirect(&target, method)
            && self
                .resolve_user_method_or_accessor(&class_name.resolve(), method)
                .is_none()
        {
            let payload_type = crate::runtime::value_type_name(&payload);
            return match self.call_method_with_values(payload, method, args) {
                // The payload has no such method either: the error names the
                // receiver (`I`), not the payload's type (`Int`) (#14318).
                Err(err) if err.is_method_not_found() && err.message.contains(&format!("of type '{payload_type}'")) => {
                    Err(make_method_not_found_error(
                        method,
                        &class_name.resolve(),
                        false,
                    ))
                }
                other => other,
            };
        }

        // Package (type object) dispatch -- private method call
        if let ValueView::Package(name) = target.view() {
            let normalized_method: String = method
                .chars()
                .map(|ch| {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ch
                    } else {
                        '_'
                    }
                })
                .collect();
            if name == "Instant"
                && normalized_method == "from_posix"
                && let Some(result) = Self::try_native_builtin_class_method(name, method, &args)
            {
                // Shared with the VM's native class-method fast path.
                return result;
            }
            // A `Systemic` class's rows that answer its type object (`Compiler.id`
            // identifies the build, not an instance) are reached here, by owner
            // (ADR-11276 §9.196). Its other methods are attribute accessors that
            // legitimately need an instance, and keep that error.
            if let Some(owners) =
                crate::builtins::method_table::opaque_class_owners(name.resolve().as_str())
                && owners.iter().any(|owner| {
                    crate::builtins::method_table::owner_row(
                        Symbol::intern(owner),
                        Symbol::intern(method),
                        args.len(),
                    )
                    .is_some_and(|id| {
                        crate::builtins::method_table::row(id)
                            .flags
                            .contains(crate::builtins::method_table::RowFlags::TYPE_OBJECT_OK)
                    })
                })
                && let Some(result) = crate::builtins::method_table::invoke_owner(
                    self,
                    owners,
                    method,
                    &args,
                    || target.clone(),
                )
            {
                return result;
            }
            // `IO::Notification.watch-path` -- shared with the VM's native
            // class-method lane.
            if let Some(result) = self.try_io_notification_class_method(name, method, &args) {
                return result;
            }
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self.private_calling_package();
                // Split at the LAST `::` so a nested owner class name
                // (`$c!Jar::Cookie::secret` → owner `Jar::Cookie`, method `secret`)
                // resolves correctly.
                let (pm_name, resolved, owner_only) = if let Some((owner_class, pm_name)) =
                    crate::qualified::split_qualified(crate::qualified::known_symbol(private_rest))
                        .map(|(head, tail)| (head.as_str(), tail.as_str()))
                {
                    // Canonicalize the source-written owner name relative to
                    // the caller's package chain before using it both for
                    // the trust check and the MRO lookup below — a short
                    // name never matches a fully qualified MRO entry.
                    let invocant_class = name.resolve();
                    let (canonical_owner, caller_allowed) = self
                        .resolve_and_check_private_owner_on(
                            caller_class.as_deref(),
                            owner_class,
                            Some(&invocant_class),
                        );
                    // Same lexical fallback as the instance path above.
                    if !caller_allowed && !self.lexical_self_is_private_owner(&canonical_owner) {
                        return Err(make_private_permission_error(
                            pm_name,
                            &canonical_owner,
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    let resolved = self.resolve_private_method_with_owner(
                        &name.resolve(),
                        &canonical_owner,
                        pm_name,
                        &args,
                    );
                    (pm_name, resolved, Some(canonical_owner))
                } else {
                    (
                        private_rest,
                        self.resolve_private_method_any_owner(&name.resolve(), private_rest, &args),
                        None,
                    )
                };
                // As on the instance path: no signature match is not "no such
                // private method". A single candidate by name runs, so its
                // binding reports the real error; several are a multi no-match.
                let resolved = match resolved {
                    Some(r) => Some(r),
                    None => {
                        let mut candidates = self.private_method_candidates_by_name(
                            &name.resolve(),
                            owner_only.as_deref(),
                            pm_name,
                        );
                        match candidates.len() {
                            0 => None,
                            1 => Some(candidates.remove(0)),
                            _ => return Err(make_multi_no_match_error(pm_name)),
                        }
                    }
                };
                if let Some((resolved_owner, method_def)) = resolved {
                    let caller_allowed = self
                        .private_owner_trusts_caller(caller_class.as_deref(), &resolved_owner)
                        || self.lexical_self_allows_private(&resolved_owner);
                    if !caller_allowed {
                        return Err(make_private_permission_error(
                            pm_name,
                            &name.resolve(),
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    let attrs = AttrMap::new();
                    let (result, _updated) = self.run_resolved_method_compiled_or_treewalk(
                        &name.resolve(),
                        &resolved_owner,
                        pm_name,
                        method_def,
                        attrs,
                        args,
                        Some(target.clone()),
                    )?;
                    return Ok(result);
                }
                return Err(make_method_not_found_error(pm_name, &name.resolve(), true));
            }
            // Package (type object) dispatch -- check user-defined methods
            if self.grammar_has_user_method(&name.resolve(), method)
                || self.package_has_applicable_user_method(&target, method, &args)
            {
                // ADR-0019 F6: VM-level direct-dispatch path first (see
                // `try_dispatch_compiled_method_direct`'s doc comment).
                if let Some(result) =
                    self.try_dispatch_compiled_method_direct(&target, method, &args)
                {
                    return result;
                }
                let attrs = AttrMap::new();
                let (result, _updated) = self.run_instance_method_at(
                    "instanceops",
                    &name.resolve(),
                    attrs,
                    method,
                    args,
                    None,
                )?;
                return Ok(result);
            }
        }

        // Value-type dispatch for user-defined methods (e.g. `augment class Array/Hash/List`).
        // Non-instance values still need to find methods declared on their type object.
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) {
            // ADR-0019 E1b: authoritative TypeId classifier owner (was
            // `value_type_name`) — see `Interpreter::dispatch_owner_name` and
            // `todo/deep/adr0019-e1-typeid-receiver-owner.md`.
            let class_name = self.dispatch_owner_name(&target);
            let dispatch_class = if self.has_user_method(class_name, method) {
                Some(class_name)
            } else if matches!(target.view(), ValueView::Array(_, kind) if !kind.is_itemized())
                && self.has_user_method("Array", method)
            {
                // @-sigiled values are list-like internally, but augmenting Array methods
                // should still apply to them.
                Some("Array")
            } else if matches!(
                target.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                // Callables follow the MRO Sub -> Routine -> Block -> Code -> Callable.
                // A method augmented onto any ancestor (e.g. `augment class Block`)
                // must still be found when invoked on a concrete Sub/Block value.
                ["Routine", "Block", "Code", "Callable"]
                    .into_iter()
                    .find(|ancestor| self.has_user_method(ancestor, method))
            } else {
                None
            };
            if let Some(dispatch_class) = dispatch_class {
                // ADR-0019 F6: VM-level direct-dispatch path first, resolving
                // against the explicit `dispatch_class` (which may differ
                // from `target`'s own runtime type — see
                // `try_dispatch_compiled_method_direct_as`'s doc comment).
                if let Some(result) = self.try_dispatch_compiled_method_direct_as(
                    crate::symbol::Symbol::intern(dispatch_class),
                    &target,
                    method,
                    &args,
                ) {
                    return result;
                }
                let attrs = AttrMap::new();
                let (result, _updated) = self.run_instance_method_at(
                    "instanceops",
                    dispatch_class,
                    attrs,
                    method,
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }
        }

        // .can for Package values
        if method == "can"
            && !args.is_empty()
            && let ValueView::Package(class_name) = target.view()
        {
            let method_name = args[0].to_string_value();
            if (self.class_has_method(&class_name.resolve(), &method_name)
                || self.has_user_method(&class_name.resolve(), &method_name))
                && let Some(val) = self.classhow_find_method(&target, &method_name)
            {
                return Ok(Value::array(vec![val]));
            }
            return Ok(Value::array(Vec::new()));
        }

        // A user class `is Range` inherits Range's own private introspection
        // attributes (`$!excludes-min`, `$!excludes-max`, `$!infinite`,
        // `$!is-int`), which real Rakudo's `.bless` never actually populates
        // for a subclass — verified against `raku`: even an explicit
        // `MyRange.bless(excludes-min => True, min => 1, max => 5)` still
        // answers `False` from `.excludes-min` (only `min`/`max` bind, since
        // those are plain positional-attribute names Range's own accessor
        // methods read straight back; the boolean predicates are computed by
        // Range's real `new`, which `.bless` bypasses). So these four
        // zero-arg predicates always resolve to their class-declared default
        // when the subclass does not override them: ordinary inherited-method
        // dispatch, which must win over `handles`/`FALLBACK` interception,
        // not be swallowed by it. Without this, calling one of them on an
        // un-overridden `is Range` subclass fell all the way through to
        // `FALLBACK` (or `X::Method::NotFound` with none defined) instead of
        // the inherited default (tokuhirom/mutsu#8807).
        if args.is_empty()
            && matches!(
                method,
                "excludes-min" | "excludes-max" | "infinite" | "is-int"
            )
            && let ValueView::Instance { class_name, .. } = target.view()
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|s| s.as_str() == "Range")
        {
            return Ok(Value::FALSE);
        }

        // Wildcard delegation (`handles *`) and FALLBACK method dispatch.
        // Dispatch order: wildcard delegation -> FALLBACK -> built-in fallbacks -> error.
        {
            let fallback_class = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(name) => Some(name),
                _ => None,
            };
            if let Some(ref class_name) = fallback_class {
                // Try wildcard delegation: forward to the delegate object,
                // resolved either from an attribute (`has $.t handles *`) or
                // by invoking a method on `target` (`method inner() handles
                // *` — registered as `"&inner"`, mirroring
                // `forward_resolved_delegation`'s explicit-list handling).
                let wildcard_attrs = self.collect_wildcard_handles(&class_name.resolve());
                if let ValueView::Instance { attributes, .. } = target.view() {
                    for attr_var in &wildcard_attrs {
                        // Check for regex delegation pattern: "attr_var:regex:pattern"
                        if let Some(regex_idx) = attr_var.find(":regex:") {
                            let real_attr = &attr_var[..regex_idx];
                            let pattern = &attr_var[regex_idx + ":regex:".len()..];
                            // Check if method name matches the Raku regex pattern first
                            // (cheap, no re-entrant call) before resolving the
                            // delegate.
                            // `parse_regex` caches the compiled pattern, so this
                            // compiles once per distinct pattern.
                            let matches = self.regex_find_first(pattern, method).is_some();
                            if !matches {
                                continue;
                            }
                            let delegate = if let Some(source_method) = real_attr.strip_prefix('&')
                            {
                                self.call_method_with_values(
                                    target.clone(),
                                    source_method,
                                    Vec::new(),
                                )
                                .ok()
                            } else {
                                let attr_key =
                                    real_attr.trim_start_matches('!').trim_start_matches('.');
                                // Clone the delegate out *in its own statement* so
                                // the attribute read guard is released before the
                                // (re-entrant) method call below — a let-chain
                                // condition keeps its temporaries (the guard) alive
                                // for the whole `if` body, which would deadlock
                                // when the callee writes back to this same
                                // instance's cell.
                                attributes.as_map().get(attr_key).cloned()
                            };
                            if let Some(delegate) = delegate {
                                match self.call_method_with_values(delegate, method, args.clone()) {
                                    Ok(val) => return Ok(val),
                                    // A failure raised BY the delegate's method
                                    // propagates; only "the delegate has no such
                                    // method" moves on to the next delegate.
                                    Err(e) if e.is_method_not_found_for(method) => continue,
                                    Err(e) => return Err(e),
                                }
                            }
                            continue;
                        }
                        let delegate = if let Some(source_method) = attr_var.strip_prefix('&') {
                            self.call_method_with_values(target.clone(), source_method, Vec::new())
                                .ok()
                        } else {
                            let attr_key = attr_var.trim_start_matches('!').trim_start_matches('.');
                            // Clone out before calling (release the read guard first).
                            attributes.as_map().get(attr_key).cloned()
                        };
                        if let Some(delegate) = delegate {
                            // Try calling the method on the delegate; if it succeeds, return
                            match self.call_method_with_values(delegate, method, args.clone()) {
                                Ok(val) => return Ok(val),
                                // See the regex arm above: only a missing method
                                // falls through; the delegate's own error propagates.
                                Err(e) if e.is_method_not_found_for(method) => continue,
                                Err(e) => return Err(e),
                            }
                        }
                    }
                }

                // Try user-defined FALLBACK method. `.REPR` is a universal
                // (Mu) pseudo-method that every object answers itself, so a
                // class's FALLBACK must never see it (Gnome::N's native-sub
                // setup does `self.package.REPR` on a class with a FALLBACK).
                if method != "FALLBACK"
                    && !(method == "REPR" && args.is_empty())
                    && self.has_user_method(&class_name.resolve(), "FALLBACK")
                {
                    let mut fallback_args = vec![Value::str(method.to_string())];
                    fallback_args.extend(args);
                    return self.call_method_with_values(target, "FALLBACK", fallback_args);
                }
            }
        }

        // Class-level attribute access (our $.x / my $.x) — works on both
        // type objects (Foo.bar) and instances ($obj.bar).
        // A per-instance attribute with the same name hides the class-level one.
        {
            let class_name_for_lookup = match target.view() {
                ValueView::Package(name) => Some(name.resolve()),
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name_for_lookup
                && args.is_empty()
                && self.has_class_level_attr(&cn, method)
                && !self.has_public_accessor(&cn, method)
                && let Some(val) = self.get_class_level_attr(&cn, method)
            {
                return Ok(val);
            }
        }

        if let Some(callable) = self.env.get(&format!("&{}", method)).cloned()
            && matches!(
                callable.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            return self.call_sub_value(callable, args);
        }

        // The `Code` rows of a regex and the `Routine` rows of a code-class
        // instance (`.^methods`' `Method` objects), found by their owner
        // (ADR-12523, ADR-11276 §9.121).
        if let Some(answer) =
            crate::builtins::method_table::code::answer(self, &target, method, &args)
        {
            return answer;
        }
        // `Mu`'s and `Cool`'s late rows: `gist`, `raku`, `Str`, `isa`, `REPR`,
        // ... of a receiver every answer above declined, from the first late
        // row of its MRO that binds the call (ADR-11276 §9.138).
        if MU_LATE_BY_MRO.contains(&method) {
            let mut mro = self.classhow_mro_unhidden_names(&target);
            // Every type is a `Mu`.
            if mro.last().map(String::as_str) != Some("Mu") {
                mro.push("Mu".to_string());
            }
            let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
            if let Some(answer) = crate::builtins::method_table::invoke_mro_late(
                self, &owners, method, &args, &target,
            ) {
                return answer;
            }
        }

        // Fallback methods
        match method {
            "name"
                if args.is_empty()
                    && !matches!(
                        target.view(),
                        ValueView::Instance { class_name, .. }
                            if !Self::is_builtin_type(class_name.resolve().as_str())
                    ) =>
            {
                match target.view() {
                    // A `Code` object's name is the `Code.name` row (`method_table::code`).
                    ValueView::Routine { .. }
                    | ValueView::Sub(_)
                    | ValueView::WeakSub(_)
                    | ValueView::Regex(..)
                    | ValueView::RegexWithAdverbs(..)
                        if let Some(name) = self.code_name_value(&target) =>
                    {
                        Ok(name)
                    }
                    // `.name` on a *type object* whose class declares its own public
                    // attribute `$.name` resolves to that accessor, and reading an
                    // instance attribute off a type object is an error (raku). Only
                    // divert when the class actually has a `name` attribute; otherwise
                    // keep the type-name introspection behaviour.
                    ValueView::Package(name)
                        if self.has_public_accessor(&name.resolve(), "name") =>
                    {
                        Err(RuntimeError::new(format!(
                            "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                            name.resolve()
                        )))
                    }
                    // `Nil` swallows every method call. `Array`/`Hash` answer
                    // with their container descriptor's name in rakudo
                    // (`[1].name` is "element", `(my %h).name` is "%h"),
                    // which mutsu does not model yet.
                    ValueView::Nil | ValueView::Hash(_) => Ok(Value::NIL),
                    ValueView::Array(..) if crate::runtime::value_type_name(&target) == "Array" => {
                        Ok(Value::NIL)
                    }
                    // `Code`'s `name` reads an attribute, which a `Code` type
                    // object does not have.
                    ValueView::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Code"
                                | "Block"
                                | "Routine"
                                | "Sub"
                                | "Method"
                                | "Submethod"
                                | "Regex"
                                | "Macro"
                                | "Array"
                        ) =>
                    {
                        Err(RuntimeError::new(format!(
                            "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                            name.resolve()
                        )))
                    }
                    // Only `Code` and the container descriptors declare
                    // `name` (`.^name` is the type-name introspection): an
                    // `Int`, a `Str`, a type object or any other value has no
                    // such method (#9776).
                    _ => Err(super::methods_signature_errors::method_not_found_for_value(
                        "name", &target,
                    )),
                }
            }
            // Metamodel::*HOW methods
            // `Metamodel::<X>HOW.new_type(:name<Foo>)`, and the same call on a
            // USER subclass of one that does not override `new_type` — rakudo
            // inherits the metamethod there, so `class BundleHOW is
            // Metamodel::ParametricRoleHOW { }; BundleHOW.new_type(...)` mints a
            // type rather than dying with "No such method 'new_type'".
            "new_type"
                if matches!(target.view(), ValueView::Package(n)
                    if n.as_str().starts_with("Metamodel::")
                        || self.is_metamodel_how_class(&n.resolve())) =>
            {
                let how_class = match target.view() {
                    ValueView::Package(sym) => sym.resolve(),
                    _ => unreachable!("guarded by the match above"),
                };
                self.metamodel_new_type(&how_class, &args)
            }
            // `Metamodel::ClassHOW.new.new_type(...)`: a builtin HOW instance
            // mints the same type its type object does.
            "new_type"
                if matches!(target.view(), ValueView::Instance { class_name, .. }
                    if Self::is_metamodel_class_name(&class_name.resolve())
                        || self.is_metamodel_how_class(&class_name.resolve())) =>
            {
                let how_class = match target.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => unreachable!("guarded by the match above"),
                };
                self.metamodel_new_type(&how_class, &args)
            }
            // Metamodel::Primitives static methods
            _ if matches!(target.view(), ValueView::Package(n) if n == "Metamodel::Primitives") => {
                self.metamodel_primitives_dispatch(method, args)
            }
            _ => {
                // A Block/Sub has no method of this name. Raku curries a method
                // call only on a WhateverCode *expression*, and the compiler does
                // that (`(* - *).abs` is one WhateverCode, `whatever_curry`); a
                // runtime Code value just lacks the method (#11445).
                if matches!(target.view(), ValueView::Sub(_) | ValueView::WeakSub(_)) {
                    // ADR-0070: an adverb the method does not accept must not be
                    // what makes the call miss. `{ $_ }.arity(:zzz)` answers 0 in
                    // raku (the `args.is_empty()` guard on the real `.arity` arm
                    // had declined). Drop the undeclared nameds and re-dispatch;
                    // `strip_undeclared_nameds` answers `Some` only when it
                    // actually removed something, so the retry cannot loop.
                    if let Some(stripped) = crate::builtins::strip_undeclared_nameds(method, &args)
                    {
                        return self.call_method_with_values(target, method, stripped);
                    }
                }

                // Before giving up, check if this is a mutating array method
                // and we have a pending Proxy subclass attribute reference.
                if matches!(target.view(), ValueView::Array(..))
                    && self.types.pending_proxy_subclass_attr.is_some()
                    && crate::builtins::method_table::names_a_mut_row(
                        Symbol::intern(method),
                        args.len(),
                    )
                    && let Some((attrs_ref, attr_name)) =
                        self.types.pending_proxy_subclass_attr.take()
                {
                    return self.proxy_subclass_array_mutate(&attrs_ref, &attr_name, method, &args);
                }

                // `CALL-ME` on a Method/Submethod `Instance` (`.^lookup`/
                // `.^find_method`/`.^methods` results -- ADR-0019 Phase F box
                // F1's Sub-vs-Instance unification,
                // `todo/tickets/classhow-lookup-returns-sub-not-method-
                // instance.md`): a non-dispatcher candidate carries its
                // callable `Sub`/`Routine` directly under
                // `__mutsu_method_callable` (`make_method_object_with_owner`/
                // `make_native_method_object`); a multi dispatcher has none
                // and instead re-dispatches on the first argument as
                // invocant, mirroring the old Sub-shaped dispatcher's
                // `sub_multi_method_dispatcher_name` re-dispatch (verified
                // against `raku`: calling a multi's `.^lookup` result
                // re-resolves by argument type).
                if method == "CALL-ME"
                    && let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = target.view()
                    && self.is_method_object_class(&class_name.resolve())
                {
                    let am = attributes.as_map();
                    // A Method object obtained from a method table carries the
                    // declaring class and method name alongside its callable
                    // payload. Re-enter ordinary method dispatch for that
                    // shape so the body's compiled method frame (including
                    // its final expression and attribute-cell handling) is
                    // used. Calling the payload directly falls back to the
                    // AST carrier and loses the value of a final attribute
                    // assignment (`.^private_method_table<foo>($obj)`).
                    let is_multi_candidate = matches!(
                        am.get("multi").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let is_method_table_entry = matches!(
                        am.get("__mutsu_method_table_entry").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    if (is_method_table_entry
                        || (matches!(class_name.resolve().as_str(), "Method" | "Submethod")
                            && matches!(
                                am.get("__mutsu_lookup_class").map(Value::view),
                                Some(ValueView::Str(owner))
                                    if args.first().is_some_and(|inv| (matches!(inv.view(), ValueView::Instance { .. })
                                        // A type object of a user class binds the
                                        // same way; a core owner's type object
                                        // (`Map.^lookup('new')(MyMap)`) re-enters
                                        // the subclass's own `new` by name.
                                        || (matches!(inv.view(), ValueView::Package(_))
                                            && (self.has_class(owner.as_str()) || self.is_role(owner.as_str()))))
                                        && self.type_matches_value(owner.as_str(), inv))
                            )))
                        && !is_multi_candidate
                        && let Some(ValueView::Str(method_name)) =
                            am.get("__mutsu_lookup_method").map(Value::view)
                        && !args.is_empty()
                    {
                        let mut args = args;
                        let invocant = args.remove(0);
                        let is_private = matches!(
                            am.get("__mutsu_private_method").map(Value::view),
                            Some(ValueView::Bool(true))
                        );
                        let method_name = if is_private {
                            format!("!{}", method_name.as_str())
                        } else {
                            method_name.to_string()
                        };
                        if is_private
                            && let Some(ValueView::Str(owner)) =
                                am.get("__mutsu_lookup_class").map(Value::view)
                        {
                            // Calling a private method through its reflection
                            // table is explicitly authorized by the table
                            // itself, even when the caller is outside the
                            // declaring class. Give the normal private
                            // resolver that lexical owner context so it uses
                            // the compiled method path without weakening
                            // ordinary source-level privacy checks.
                            self.push_method_class_authorizing_private(&owner.to_string());
                            let result = self.call_method_with_values(invocant, &method_name, args);
                            self.pop_method_class();
                            return result;
                        }
                        // The entry is its owner's candidate, not whatever
                        // the invocant's class overrides it with (#10344).
                        if !is_private
                            && let Some(ValueView::Str(owner)) =
                                am.get("__mutsu_lookup_class").map(Value::view)
                            && let Some(qualified) = self.owner_bound_method_name(
                                Symbol::intern(&owner),
                                Symbol::intern(&method_name),
                                &invocant,
                            )
                        {
                            return self.call_method_with_values(
                                invocant,
                                qualified.as_str(),
                                args,
                            );
                        }
                        return self.call_method_with_values(invocant, &method_name, args);
                    }
                    if let Some(result) = self.try_call_bound_method_object(&target, &args) {
                        return result;
                    }
                    if let Some(callable) = am.get("__mutsu_method_callable").cloned() {
                        return self.call_sub_value(callable, args);
                    }
                    if matches!(
                        am.get("is_dispatcher").map(Value::view),
                        Some(ValueView::Bool(true))
                    ) && let Some(ValueView::Str(meth)) =
                        am.get("__mutsu_lookup_method").map(Value::view)
                        && !args.is_empty()
                    {
                        let mut args = args;
                        let invocant = args.remove(0);
                        // Bound to the owner's candidates, like the entry
                        // above: a by-name re-dispatch would ask the
                        // receiver's method lookup again, which a user
                        // `^find_method` answers with this same dispatcher.
                        if let Some(ValueView::Str(owner)) =
                            am.get("__mutsu_lookup_class").map(Value::view)
                            && let Some(qualified) = self.owner_bound_method_name(
                                Symbol::intern(&owner),
                                Symbol::intern(meth.as_str()),
                                &invocant,
                            )
                        {
                            return self.call_method_with_values(
                                invocant,
                                qualified.as_str(),
                                args,
                            );
                        }
                        return self.call_method_with_values(invocant, &meth.to_string(), args);
                    }
                }

                // Metamodel method fallback (`.^add_fallback`): before failing,
                // consult any dynamic fallbacks registered for this value's class.
                if let Some(result) = self.try_method_fallback(&target, method, &args) {
                    return result;
                }
                // A user HOW subclass (`class MyHOW is Metamodel::ClassHOW`)
                // inherits the native ClassHOW meta-methods (`methods`, `wrap`
                // sits on Method, `add_method`, ...): when the user class does
                // NOT override the method, dispatch it to the native ClassHOW
                // implementation. The invocant is the meta-object; the native
                // impl operates on its first arg (the type object), so pass the
                // call args straight through (e.g. `self.methods($obj, :local)`
                // in a custom `compose` — `advent2011-day14` AOP).
                if let ValueView::Instance { class_name, .. } = target.view()
                    && Self::is_classhow_method(method)
                    && self.is_metamodel_how_class(&class_name.resolve())
                {
                    let mut how_args = args.to_vec();
                    // `$how.archetypes` takes the type optionally: a metaobject
                    // installed for a class answers about that class.
                    if how_args.is_empty() {
                        let described = Self::how_target_from_value(&target)
                            .unwrap_or_else(|| "Any".to_string());
                        how_args.push(Value::package(Symbol::intern(&described)));
                    }
                    return self.dispatch_classhow_method(method, how_args);
                }
                // A grammar token/rule called as an instance method
                // (`G.new.tok`, URI's `IETF::RFC_Grammar::URI.new()
                // .TOP-non-empty`): rakudo runs the token against an
                // empty-string cursor and returns the resulting Match cursor —
                // falsy when the token cannot match the empty string. Run
                // `subparse("", rule => method)`; a failed subparse yields a
                // falsy value that smartmatches False like a failed cursor.
                //
                // When `target` is ITSELF a live cursor bound to a real parse
                // in progress — a grammar's own `method TOP` delegating to
                // `self.some-rule` (issue #8752) — restarting on `""` would
                // throw away the actual text and position. Continue from the
                // cursor's own `orig`/position instead.
                if args.is_empty()
                    && let ValueView::Instance { class_name, .. } = target.view()
                {
                    let cn = class_name.resolve();
                    let qualified = crate::qualified::qualified_text(&cn, method)
                        .as_str()
                        .to_string();
                    if self
                        .resolve_token_defs(&qualified)
                        .map(|d| !d.is_empty())
                        .unwrap_or(false)
                    {
                        let mut call_args = match Interpreter::cursor_call_position(&target)
                            .or_else(|| Interpreter::subrule_invocant_position(&target))
                        {
                            Some((orig, pos, anchored)) => vec![
                                Value::str(orig),
                                Value::pair(
                                    if anchored { "pos" } else { "c" }.to_string(),
                                    Value::int(pos as i64),
                                ),
                            ],
                            None => vec![Value::str_from("")],
                        };
                        call_args.push(Value::pair(
                            "rule".to_string(),
                            Value::str(method.to_string()),
                        ));
                        let result = self.dispatch_package_parse(&cn, "subparse", &call_args)?;
                        // Remember which rule produced this Match, so a
                        // method-shaped start rule that returns it fires that
                        // rule's action (`method TOP { self.rule }`).
                        if let (Some(from), Some(to)) = (result.match_from(), result.match_to())
                            && let (Ok(from), Ok(to)) = (usize::try_from(from), usize::try_from(to))
                        {
                            crate::runtime::regex::regex_token_method::record_token_delegate(
                                method, from, to,
                            );
                        }
                        return Ok(result);
                    }
                }
                // ADR-0019 E1b: authoritative TypeId classifier owner (was
                // `value_type_name`) in the fallback arm — see
                // `Interpreter::dispatch_owner_name`.
                let type_name = match target.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    ValueView::Package(name) => name.resolve(),
                    _ => self.dispatch_owner_name(&target).to_string(),
                };
                // RakuAST construction via a non-`new` constructor (Phase 4),
                // e.g. `RakuAST::Name.from-identifier("x")` (`.new` is handled
                // earlier in methods_object_dispatch_new).
                if type_name.starts_with("RakuAST::")
                    && let Some(node) = crate::rakuast::construct(&type_name, method, &args)?
                {
                    return Ok(node);
                }
                // A field read on an `is repr('CStruct')` handle: the instance
                // carries the C pointer, so resolve the name against the
                // struct's declared field layout and read it out of native
                // memory (`$ssl.server`, `nativecast(evp_cipher_st, $c).key_len`).
                if args.is_empty()
                    && let Some(field) = self.cstruct_field_value(&target, method)
                {
                    return Ok(field);
                }
                // A bare type object's own instance-attribute accessor is
                // meaningless (there is no instance to read from), so
                // `user_shadows_builtin`'s type-object branch never
                // routes here through the accessor and the call falls all
                // the way to "no such method" instead. raku instead resolves
                // to the real accessor and lets ITS body raise this error
                // when it tries to read attribute storage that does not
                // exist — reproduce that outcome directly for any public
                // accessor name, generalizing the `.name`-only special case
                // above (which needs its own arm since `.name` also has a
                // type-name-introspection meaning).
                if let ValueView::Package(name) = target.view()
                    && args.is_empty()
                    && self.has_public_accessor(&name.resolve(), method)
                {
                    // A `.wrap`ped accessor runs its wrapper first, as for an
                    // instance (`dispatch_instance_and_fallback`): the wrapper
                    // may supply an instance (`callwith($obj)`, Staticish's
                    // singleton wrapper), and only the accessor itself needs one.
                    let class_name = name.resolve();
                    if let Some(owner) = self.attribute_accessor_owner(&class_name, method)
                        && let Some(chain) =
                            self.get_method_wrap_chain(owner.resolve().as_str(), method, 0)
                    {
                        return self.dispatch_wrapped_attribute_accessor(
                            target.clone(),
                            &class_name,
                            args,
                            chain,
                            super::DeferralEntry::Accessor {
                                owner,
                                name: method.to_string(),
                                want_container: false,
                            },
                        );
                    }
                    return Err(RuntimeError::new(format!(
                        "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                        name.resolve()
                    )));
                }
                // A built-in method the receiver has, called with more
                // positionals than any candidate takes, fails to bind.
                if let Some(receiver) = crate::builtins::method_table::Receiver::of(&target)
                    && let Some(max) = crate::builtins::method_table::max_row_arity(
                        receiver,
                        Symbol::intern(method),
                    )
                    // Named arguments arrive as pairs and bind no positional.
                    && args.iter().filter(|arg| !arg.is_string_pair_value()).count() > max
                {
                    let expected = max + 1;
                    let got = args.iter().filter(|arg| !arg.is_string_pair_value()).count();
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected {expected} argument{} but got {}",
                        if expected == 1 { "" } else { "s" },
                        got + 1
                    )));
                }
                // `Any`'s `invert` (`self.list.invert`) reaches a `Capture` and a
                // `Version`, which have no row of their own for it.
                // TODO: compile to bytecode -- give `Any` a real `invert` row
                // and let `DispatchShape::reaches` open it.
                match (method, target.view()) {
                    ("invert", ValueView::Capture { positional, .. }) if args.is_empty() => {
                        let items = Value::real_array(positional.to_vec());
                        return self.call_method_with_values(items, method, args);
                    }
                    ("invert", ValueView::Version { .. }) if args.is_empty() => {
                        let items = Value::real_array(vec![target.clone()]);
                        return self.call_method_with_values(items, method, args);
                    }
                    _ => {}
                }
                // A subset's type object answers the methods of its base
                // (`my subset S of R; S.COERCE(..)` reaches `R`'s).
                let subset_base = match target.view() {
                    ValueView::Package(name) => {
                        let key = name.resolve();
                        let base = self
                            .registry()
                            .subsets
                            .get(key.as_str())
                            .map(|d| d.base.clone());
                        base.filter(|b| *b != key)
                    }
                    _ => None,
                };
                if let Some(base) = subset_base {
                    return self.call_method_with_values(
                        Value::package(Symbol::intern(&base)),
                        method,
                        args,
                    );
                }
                // Rakudo words a `Str` invocant's miss `for string 'abc'`.
                let text = match target.view() {
                    ValueView::Str(s) => Some(s.to_string()),
                    ValueView::Package(name) if name.resolve() == "Str" => Some(String::new()),
                    _ => None,
                };
                Err(super::methods_signature_errors::make_method_not_found_error_for(
                    method,
                    &type_name,
                    false,
                    text.as_deref(),
                ))
            }
        }
    }

    /// Consult metamodel method fallbacks registered via `.^add_fallback` for a
    /// value whose method lookup just failed. Returns `Some(result)` when a
    /// fallback's condition matched (and its calculated method body was invoked),
    /// or `None` when no fallback applies (the caller then raises NotFound).
    fn try_method_fallback(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if self.types.method_fallbacks.is_empty() {
            return None;
        }
        // ADR-0019 E1b: authoritative TypeId classifier owner (was
        // `value_type_name`) in the fallback arm — see
        // `Interpreter::dispatch_owner_name`.
        let type_name = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => self.dispatch_owner_name(target).to_string(),
        };
        // Consult the class itself first, then its ancestors (MRO).
        let mut classes = vec![type_name.clone()];
        classes.extend(
            self.class_mro(&type_name)
                .iter()
                .map(|s| s.resolve())
                .filter(|c| c != &type_name),
        );
        let name_val = Value::str(method.to_string());
        for cn in classes {
            let Some(fallbacks) = self.types.method_fallbacks.get(&cn).cloned() else {
                continue;
            };
            for (condition, calculator) in fallbacks {
                let cond = match self.call_sub_value(
                    condition,
                    vec![target.clone(), name_val.clone()]) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                if !cond.truthy() {
                    continue;
                }
                // The calculator returns the method body (a Callable) for this
                // name; invoke it with the invocant (and any call args).
                let method_code =
                    match self.call_sub_value(calculator, vec![target.clone(), name_val]) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                let mut call_args = vec![target.clone()];
                call_args.extend_from_slice(args);
                return Some(self.call_sub_value(method_code, call_args));
            }
        }
        None
    }

    /// Mutate an array attribute in a Proxy subclass's shared storage: the
    /// `Array` mutator rows on the stored array as a detached place
    /// (ADR-11276 §9.147), written back so every alias of the storage sees it.
    // Cost: the row's own cost, plus one lock round trip.
    pub(crate) fn proxy_subclass_array_mutate(
        &mut self,
        attrs_ref: &crate::value::ProxySubclassAttrs,
        attr_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // The lock is not held across the row, which may run user code.
        let mut array = attrs_ref
            .as_map()
            .get(attr_name)
            .cloned()
            .unwrap_or_else(|| Value::real_array(Vec::new()));
        let answer = crate::builtins::method_table::invoke_mut(
            self,
            &mut crate::builtins::method_table::ReceiverPlace::Detached(&mut array),
            Symbol::intern(method),
            args,
        );
        let Some(answer) = answer else {
            return Err(RuntimeError::new(format!(
                "Cannot call '{}' on non-Array attribute",
                method
            )));
        };
        attrs_ref.set_attribute_value(Symbol::intern(attr_name), array);
        answer
    }

    pub(crate) fn dispatch_are(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // `.are` walks the invocant's `iterator`, so a class with its own
        // (a `does Positional` linked list) is judged by the elements it
        // yields, not as one item (Functional::LinkedList via ValueClass's
        // `data.are !~~ $attr.type.of`).
        let values = match self.try_user_iterator_items(&target)? {
            Some(items) => items,
            None => Self::value_to_list(&target),
        };
        match args {
            [] => {
                if values.is_empty() {
                    return Ok(Value::NIL);
                }
                let candidates = self.are_candidate_type_names(&values);
                for candidate in candidates {
                    if values
                        .iter()
                        .all(|value| self.type_matches_value(&candidate, value))
                    {
                        return Ok(Value::package(Symbol::intern(&candidate)));
                    }
                }
                Ok(Value::package(crate::symbol::wk::any()))
            }
            [expected] => {
                let expected_type = self.are_expected_type_name(expected);
                for (idx, value) in values.iter().enumerate() {
                    if !self.type_matches_value(&expected_type, value) {
                        let actual = self.are_actual_type_name(value);
                        let message = if values.len() == 1 {
                            format!("Expected '{}' but got '{}'", expected_type, actual)
                        } else {
                            format!(
                                "Expected '{}' but got '{}' in element {}",
                                expected_type, actual, idx
                            )
                        };
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(message));
                        let ex = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Ok(Value::make_instance(
                            Symbol::intern("Failure"),
                            failure_attrs,
                        ));
                    }
                }
                Ok(Value::TRUE)
            }
            _ => Err(RuntimeError::new(
                "Method 'are' accepts zero or one argument",
            )),
        }
    }

    fn are_candidate_type_names(&mut self, values: &[Value]) -> Vec<String> {
        let mut names = Vec::new();
        for value in values {
            for candidate in self.are_specific_candidate_type_names(value) {
                if !names.contains(&candidate) {
                    names.push(candidate);
                }
            }
        }
        for fallback in ["Dateish", "Real", "Numeric", "Cool", "Any"] {
            let fallback = fallback.to_string();
            if !names.contains(&fallback) {
                names.push(fallback);
            }
        }
        names
    }

    fn are_specific_candidate_type_names(&mut self, value: &Value) -> Vec<String> {
        match value.view() {
            ValueView::Package(name) => vec![name.resolve()],
            // The `Any`/`Mu` that close an instance's MRO (as opposed to being
            // its own type, `Mu.new`) are the LAST resort:
            // they must not outrank the shared-role fallbacks the caller
            // appends (`(DateTime.now, Date.today).are` is `Dateish`).
            ValueView::Instance { class_name, .. } => self
                .class_mro(&class_name.resolve())
                .iter()
                .enumerate()
                .filter(|(i, s)| *i == 0 || (s.as_str() != "Any" && s.as_str() != "Mu"))
                .map(|(_, s)| s.resolve())
                .collect(),
            // ADR-0019 E1b: authoritative TypeId classifier owner (was
            // `value_type_name`) — see `Interpreter::dispatch_owner_name`.
            _ => vec![self.dispatch_owner_name(value).to_string()],
        }
    }

    fn are_expected_type_name(&self, value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Str(name) => name.to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => value.to_string_value(),
        }
    }

    fn are_actual_type_name(&mut self, value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            // ADR-0019 E1b: authoritative TypeId classifier owner (was
            // `value_type_name`) — see `Interpreter::dispatch_owner_name`.
            _ => self.dispatch_owner_name(value).to_string(),
        }
    }

    /// Collect public attribute representations for `.raku` output.
    /// Returns a list of `"name => value.raku"` strings for public attributes.
    pub(crate) fn collect_public_raku_attrs(
        &mut self,
        class_name: &str,
        attributes: &AttrMap,
    ) -> Vec<String> {
        let class_attrs = self.collect_class_attributes_display_order(class_name);
        // A punned role is materialized as a temporary class during `.new`, then
        // withdrawn so its type object remains a role. Its instance still carries
        // the role's attribute values, so render from the composed role metadata
        // when the class shell is no longer in the registry.
        let class_attrs =
            if class_attrs.is_empty() && self.registry().roles.contains_key(class_name) {
                self.collect_role_attributes_for_class(class_name)
            } else {
                class_attrs
            };
        let mut parts = Vec::new();
        for attr in &class_attrs {
            let (attr_name, is_public, sigil) = (&attr.name, attr.is_public, attr.sigil);
            if !is_public {
                continue;
            }
            // Rakudo's default `.raku` lists only the attributes `.new` can
            // set: a public `is built(False)` attribute is left out.
            if !self.is_attribute_buildable(class_name, attr_name) {
                continue;
            }
            // Reading the attribute to render it vivifies it, as Rakudo's
            // `.raku` does through `Attribute.get_value` (#11003).
            if let Some(val) = attributes.get_vivify(attr_name.as_str()) {
                // Render what the slot holds, not the cell a `:=` bind or an
                // `=` value share (Slice 2e) put around it.
                let val = &val.deref_container();
                // The cycle-guarded dispatch renders a direct instance-valued
                // attribute that refers back up the chain (`$x.next = $y;
                // $y.next = $x`) as a backreference instead of recursing.
                let rendered = self
                    .dispatch_raku_leaf(val)
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| val.to_string_value());
                // A `$`-sigil attribute is a Scalar container, so an aggregate
                // it holds renders itemized: `Foo.new(x => $[1, 2])`. `@`/`%`
                // attributes are the aggregate itself and stay bare.
                let rendered = if sigil == '$' {
                    crate::builtins::methods_0arg::raku_repr::itemize_scalar_repr(val, rendered)
                } else {
                    rendered
                };
                parts.push(format!("{} => {}", attr_name, rendered));
            }
        }
        parts
    }

    /// The native attribute-copying clone of an `Instance` value, with `:attr(v)`
    /// twiddle args applied. Shared between the direct `.clone` dispatch arm and
    /// the `callsame` base candidate of a user `clone` override
    /// (`native_mu_base`). `None` when the value is not an
    /// Instance.
    // Cost: O(a) in the instance's attribute count.
    pub(crate) fn native_instance_clone_value(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let mut attrs = attributes.to_clone_map();
        // Build sigil map from class attributes to coerce values properly
        let class_attrs_info = self.collect_class_attributes(&class_name.resolve());
        let sigil_map: HashMap<String, char> = class_attrs_info
            .iter()
            .map(|attr| (attr.name.clone(), attr.sigil))
            .collect();
        let cn = class_name.resolve();
        for arg in args {
            if let ValueView::Pair(key, boxed) = arg.view()
                && self.is_attribute_buildable(&cn, key)
            {
                let sigil = sigil_map.get(key.as_str()).copied().unwrap_or('$');
                let coerced = Self::coerce_attr_value_by_sigil(boxed.clone(), sigil);
                attrs.insert(key.clone(), coerced);
            }
        }
        Some(Ok(Value::make_instance(class_name, attrs)))
    }

    /// Whether a routine body is exactly a yada stub (`...`/`!!!`/`???`), which
    /// parses to a single `__mutsu_stub_die`/`__mutsu_stub_warn` call. `SetLine`
    /// annotations don't count; anything beyond the lone stub makes it non-yada
    /// (`sub f { ...; 2 }` is not a stub).
    pub(super) fn sub_body_is_yada_stub(body: &[crate::ast::Stmt]) -> bool {
        use crate::ast::{Expr, Stmt};
        let meaningful: Vec<&Stmt> = body.iter().filter(|s| !s.is_marker()).collect();
        meaningful.len() == 1
            && matches!(meaningful[0], Stmt::Expr(Expr::Call { name, .. })
                if crate::ast::stub::is_marker(name.as_str()))
    }
}

/// Format an operator name for display.
///
/// Raku uses different delimiters depending on the operator symbol:
/// - If the symbol contains characters special inside `\u{ab}\u{bb}` quoting
///   (`$`, `@`, `%`, `&`, `{`, `\`, `\u{ab}`, `\u{bb}`), use `<>` delimiters
///   and backslash-escape any `<` or `>` in the symbol.
/// - Otherwise, if the symbol contains `<` or `>`, use `\u{ab}\u{bb}` delimiters.
/// - Otherwise, use `<>` delimiters as-is.
pub(super) fn format_operator_name(name: &str) -> String {
    // Check if this is a categorical name like "infix:<...>", "prefix:<...>", etc.
    let Some(colon_pos) = name.find(":<") else {
        return name.to_string();
    };
    let category = &name[..colon_pos];
    let rest = &name[colon_pos + 2..];
    // The rest should end with '>'
    let Some(symbol) = rest.strip_suffix('>') else {
        return name.to_string();
    };

    // Characters that are special inside «» quoting and force <> delimiters
    let needs_angle_delims = symbol
        .chars()
        .any(|c| matches!(c, '$' | '@' | '%' | '&' | '{' | '\\' | '\u{ab}' | '\u{bb}'));
    let has_angle = symbol.contains('<') || symbol.contains('>');

    if needs_angle_delims {
        // Must use <> delimiters; backslash-escape < and > in the symbol
        if has_angle {
            let mut escaped = String::with_capacity(symbol.len() + 4);
            for ch in symbol.chars() {
                if ch == '<' || ch == '>' {
                    escaped.push('\\');
                }
                escaped.push(ch);
            }
            format!("{}:<{}>", category, escaped)
        } else {
            // No angle brackets to escape, just use <> as-is
            name.to_string()
        }
    } else if has_angle {
        // Use french quote delimiters
        format!("{}:\u{ab}{}\u{bb}", category, symbol)
    } else {
        name.to_string()
    }
}
