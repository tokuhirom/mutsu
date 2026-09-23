//! The `nqp::` ops whose implementations need the full `Interpreter` surface
//! (method re-dispatch, the NativeCall pointer layer, the class registry) and
//! therefore cannot live in the pure value table of [`super::nqp_ops`].
//!
//! These used to be ordinary arms of `Interpreter::call_function`'s builtin
//! `match`, which meant every `nqp::` call first walked the whole dispatch
//! chain — light-call caches, proto/multi candidate scans, the native-function
//! tables — before its name could be recognized. The `nqp::` namespace is
//! reserved and never carries a user routine, so that walk was always dead
//! work; it is now short-circuited at the VM's call opcode
//! (`dispatch_nqp_op`), and this module is where the interpreter-coupled half
//! of that dispatch lives.

use super::*;
use crate::runtime::nqp_op_ids::NqpOpTable;

impl Interpreter {
    /// The single entry point for every `nqp::` op, with `op` being the name
    /// **without** the `nqp::` prefix.
    ///
    /// Ordering mirrors the historical dispatch: the interpreter-coupled ops
    /// below were matched inside `call_function` (i.e. before the builtin
    /// fallback chain), and everything else falls through to the pure value
    /// table in `nqp_ops.rs`. An op neither knows stays a loud error rather
    /// than silently reaching a same-named Raku builtin with different
    /// semantics — `nqp::index` returns -1 where Raku's `index` returns Nil,
    /// and nqp code branches on exactly that.
    pub(crate) fn dispatch_nqp_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let _region = crate::profile::enter(crate::profile::Region::Nqp);
        if let Some(result) = self.call_nqp_interpreter_op(op, args) {
            return result;
        }
        if let Some(result) = self.call_nqp_op(op, args) {
            return result;
        }
        Err(RuntimeError::new(format!(
            "Unsupported nqp:: op: nqp::{op}"
        )))
    }

    /// [`Self::dispatch_nqp_op`] for a call site whose op was already resolved
    /// to a dense registry id at COMPILE time (`OpCode::NqpOp`).
    ///
    /// The registry records which of the six chained tables claims the op, so
    /// this enters that one directly instead of walking the chain from the
    /// top: `nqp::ordat` lives in the fifth table and used to pay four failed
    /// `match op` walks — over 25, 72, 20 and 24 names — before reaching its
    /// own. Each table's `_` arm still falls through to the next, so entering
    /// mid-chain only skips walks that were going to decline.
    ///
    /// A registry entry tagged with the wrong table therefore cannot break an
    /// op, only slow it down: the tables after it decline too, and the `None`
    /// that falls out re-runs the full chain from the top.
    pub(crate) fn dispatch_nqp_op_by_id(
        &mut self,
        id: u16,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let _region = crate::profile::enter(crate::profile::Region::Nqp);
        let op = crate::runtime::nqp_op_ids::nqp_op_name(id);
        let claimed = match crate::runtime::nqp_op_ids::nqp_op_table(id) {
            NqpOpTable::Builtin => self.call_nqp_interpreter_op(op, args),
            NqpOpTable::Value => self.call_nqp_op(op, args),
            NqpOpTable::Process => self.call_nqp_op_process(op, args),
            NqpOpTable::Text => self.call_nqp_op_text(op, args),
            NqpOpTable::Str => self.call_nqp_op_str(op, args),
            NqpOpTable::List => self.call_nqp_op_list(op, args),
        };
        match claimed {
            Some(result) => result,
            None => self.dispatch_nqp_op(op, args),
        }
    }

    /// The interpreter-coupled `nqp::` ops. `None` means "not one of these" —
    /// the caller then tries the pure value table.
    fn call_nqp_interpreter_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            "atkey" => {
                let hash = args.first().cloned().unwrap_or(Value::NIL);
                let key = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                match hash.view() {
                    ValueView::Hash(map) => Ok(map.get(&key).cloned().unwrap_or(Value::NIL)),
                    _ => {
                        let h = hash.clone();
                        self.call_method_with_values(h, "AT-KEY", vec![Value::str(key)])
                    }
                }
            }
            "atpos" => {
                let list = args.first().cloned().unwrap_or(Value::NIL);
                let idx = args
                    .get(1)
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i as usize),
                        _ => v.to_string_value().parse::<usize>().ok(),
                    })
                    .unwrap_or(0);
                match crate::runtime::nqp_ops_list::nqp_backing_array(&list) {
                    Some(backing) => match backing.view() {
                        ValueView::Array(items, _) => {
                            Ok(items.get(idx).cloned().unwrap_or(Value::NIL))
                        }
                        _ => Ok(Value::NIL),
                    },
                    None => Ok(Value::NIL),
                }
            }
            // nqp::ordat($str, $pos): the Unicode codepoint of the character at
            // position `$pos` in `$str` (equivalent to `$str.substr($pos, 1).ord`).
            // Returns -1 when the position is past the end, matching nqp. Used by
            // Text::Diff::Sift4's inner char-comparison loop, and by JSON::Fast's
            // `nom-ws`, which calls it once per character while skipping
            // whitespace over the WHOLE document. Collecting `args[0]` into a
            // fresh `String`/`Vec<char>` on every call turned that into O(n) work
            // repeated O(n) times (a 30KB `License::SPDX` resource file already
            // took ~14s); memoizing via `nqp_char_cache` — the same fix already
            // applied to `substr`/`index`/`iscclass` for this exact scanner —
            // makes each call O(1) amortized.
            "ordat" => {
                let chars = super::nqp_char_cache::cached_chars(args, 0);
                let pos = args
                    .get(1)
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i),
                        _ => v.to_string_value().parse::<i64>().ok(),
                    })
                    .unwrap_or(0);
                let cp = usize::try_from(pos)
                    .ok()
                    .and_then(|p| chars.get(p))
                    .map(|&c| c as i64)
                    .unwrap_or(-1);
                Ok(Value::int(cp))
            }
            // nqp::sha1($str): the SHA-1 digest of the string's UTF-8 encoding,
            // as 40 uppercase hex digits. Needed by two components mutsu ships:
            // the vendored zef (`Zef::Distribution.id` and the source-path
            // computation) and bundled OpenSSL's `dll-resource()`.
            "sha1" => {
                let s = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(crate::builtins::sha1::sha1_hex_uppercase(
                    s.as_bytes(),
                )))
            }
            // nqp::gethostname(): the system hostname as a native str. Used by
            // Sys::Hostname's `hostname` sub (`nqp::gethostname.subst(...)`).
            "gethostname" => Ok(Value::str(Self::hostname())),
            // nqp::gethllsym($hll, $name) / nqp::bindhllsym($hll, $name, $value):
            // MoarVM's per-HLL symbol table, a global name -> value registry
            // each high-level language (here just "default", NQP's own HLL)
            // populates for the others to read. Rakudo's core setting reaches
            // for it during BEGIN-time bootstrap -- e.g.
            // `nqp::gethllsym("default","SysConfig").rakudo-build-config<version>`
            // in `Rakudo::CORE::META` (issue #8775) -- to get at compiler
            // build info before any Raku-level class is composed. mutsu seeds
            // the one binding it needs (`bootstrap_hll_syms`) and otherwise
            // treats this as a genuinely general get/set pair backed by
            // [`Interpreter::hll_syms`], not a special case for that one key.
            "gethllsym" => {
                let hll = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                Ok(self.get_hll_sym(&hll, &name))
            }
            "bindhllsym" => {
                let hll = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let value = args.get(2).cloned().unwrap_or(Value::NIL);
                self.set_hll_sym(hll, name, value.clone());
                Ok(value)
            }
            // nqp::getlexdyn($name): resolve a dynamic variable by a
            // runtime-computed name, the same way a compiled `%*NAME`/`$*NAME`
            // read would (`get_env_with_main_alias` is the chokepoint every
            // ordinary dynamic read — base-tier, user-declared, or lazily
            // materialized via `lazy_magic_dynamic_var` — already goes
            // through). This is genuinely generic, not a special case for one
            // name: NQP code reaches for it to read a dynamic whose name it
            // only has as a string, `%*COMPILING` (Rakudo::Options, issue
            // #8572) chief among them in the wild.
            "getlexdyn" => {
                let name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                match self.get_env_with_main_alias(&name) {
                    Some(v) => Ok(v),
                    None => Err(RuntimeError::new(format!(
                        "No such dynamic variable: {name}"
                    ))),
                }
            }
            // nqp::bindattr($obj, Type, '$!attr', value): write an attribute
            // cell directly, bypassing accessors (roast's Test::Compile uses it
            // to install a precomp repository into a CUR::FileSystem instance).
            // The typed variants coerce first: an nqp `int`/`num`/`str`
            // attribute holds a native value, and code that reads it back with
            // `getattr_i` expects one.
            "bindattr" | "bindattr_i" | "bindattr_n" | "bindattr_s" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let attr = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let raw = args.get(3).cloned().unwrap_or(Value::NIL);
                let val = match op {
                    "bindattr_i" => Value::int(to_int(&raw)),
                    "bindattr_n" => Value::num(raw.to_f64()),
                    "bindattr_s" => Value::str(raw.to_string_value()),
                    _ => raw,
                };
                match Self::nqp_bindattr_value(op, &obj, &attr, val.clone()) {
                    Ok(()) => Ok(val),
                    Err(e) => Err(e),
                }
            }
            // nqp::decont($x): strip the container off a value. Raku's `.item`
            // twin at the nqp level; the ops below take their argument through
            // it, so a `$`-variable argument does not arrive as a Scalar.
            "decont" => Ok(crate::runtime::types::unwrap_varref_value(
                args.first().cloned().unwrap_or(Value::NIL),
            )),
            // nqp::ifnull($a, $b): `$a` unless it is the native null sentinel,
            // else `$b`. mutsu has no separate native-null representation from
            // `Nil`, so a Nil/absent `$a` is treated as null -- the same
            // simplification `getpayload` below relies on to make its "no
            // native payload" case compose correctly with this op (the pattern
            // `nqp::ifnull(nqp::getpayload($ex), $ex)`, from Rakudo's core
            // `X::Wrapper` role, needs exactly this fallback-to-`$ex` behavior).
            "ifnull" => Ok({
                let a = args.first().cloned().unwrap_or(Value::NIL);
                if a.is_nil() {
                    args.get(1).cloned().unwrap_or(Value::NIL)
                } else {
                    a
                }
            }),
            // nqp::getpayload($ex): the payload MoarVM attached to a low-level
            // (non-Raku) exception object via `nqp::setpayload` at throw time --
            // e.g. a foreign/NativeCall exception wrapping an arbitrary value.
            // mutsu's exception values are always already-boxed Raku `Value`s,
            // with no separate native-exception-with-payload representation, so
            // there is never a distinct payload to report here.
            // TODO: if mutsu ever models a genuine native/foreign exception
            // wrapper (e.g. for a future NativeCall exception-trapping
            // feature), thread its payload through instead of always
            // reporting "none" -- see #8573.
            "getpayload" => Ok(Value::NIL),
            // nqp::getmessage($ex): the message of a low-level exception
            // object. Reuses raku's own message-derivation rules
            // (`exception_message_text`: a user `method message` wins over the
            // stored attribute) for a Raku exception instance, and falls back
            // to stringifying anything else (mutsu has no separate native
            // exception representation to introspect).
            "getmessage" => {
                let ex = args.first().cloned().unwrap_or(Value::NIL);
                let msg = self
                    .exception_message_text(&ex)
                    .unwrap_or_else(|| ex.to_string_value());
                Ok(Value::str(msg))
            }
            // nqp::backtrace($ex): the native backtrace MoarVM captured when
            // `$ex` was thrown, as the array-of-frame-hashes `Backtrace.new`
            // expects. mutsu's `Backtrace.new` does not consume that shape --
            // it always captures the *current* call stack directly (see
            // `build_backtrace_value`) -- so an empty array is a safe,
            // non-crashing placeholder for the argument; `Backtrace.new(...)`
            // ignores it and returns backtrace of the current point of the
            // program's execution.
            // TODO: thread the exception's own captured frames through here
            // once `Backtrace.new` can be constructed from an explicit frame
            // list instead of always sampling the live stack -- see #8573.
            "backtrace" => Ok(Value::array(Vec::new())),
            // nqp::unbox_i($x): the native integer inside a boxed value. A
            // NativeCall `Pointer` unboxes to its address, which is what makes
            // pointer arithmetic expressible — `NativeHelpers::Pointer` builds
            // `.add`/`.succ`/`.pred` out of exactly that.
            "unbox_i" => {
                let v = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                if let ValueView::Instance { attributes, .. } = v.view()
                    && let Some(payload) = attributes.as_map().get("__mutsu_int_value")
                {
                    return Some(Ok(Value::int(crate::runtime::to_int(payload))));
                }
                Ok(Value::int(
                    crate::runtime::nativecall::value_c_address(&v) as i64
                ))
            }
            // nqp::box_i($i, Type): the inverse. A `Pointer`/`Pointer[T]` target
            // yields a pointer at that address (`Pointer.new(0)` is a legitimate
            // defined value, so a zero address stays defined). A user subclass
            // of Int needs an instance carrying the native payload so that
            // `nqp::istype` and value coercion still see the requested type;
            // plain Int targets remain the immediate scalar below.
            "box_i" => {
                let n = args.first().map(crate::runtime::to_int).unwrap_or(0);
                let target = args
                    .get(1)
                    .map(|v| match v.view() {
                        ValueView::Package(name) => name.resolve(),
                        ValueView::Instance { class_name, .. } => class_name.resolve(),
                        _ => String::new(),
                    })
                    .unwrap_or_default();
                let short = crate::runtime::cstruct_layout::short_base_name(&target);
                if short == "Pointer" || short.starts_with("Pointer[") {
                    let of = short
                        .strip_prefix("Pointer[")
                        .and_then(|s| s.strip_suffix(']'));
                    let addr = n.max(0) as usize;
                    return Some(Ok(match of {
                        Some(of) => crate::runtime::nativecall::make_typed_pointer(addr, of),
                        None => crate::runtime::nativecall::make_pointer_object(addr),
                    }));
                }
                if !target.is_empty()
                    && target != "Int"
                    && self.class_mro(&target).iter().any(|name| name == "Int")
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("__mutsu_int_value".to_string(), Value::int(n));
                    return Some(Ok(Value::make_instance(Symbol::intern(&target), attrs)));
                }
                Ok(Value::int(n))
            }
            // nqp::create($type) — allocate an instance of `$type` with NO
            // constructor run: attributes stay uninitialized and `BUILD` is
            // never called, which is exactly Raku's `.CREATE`. nqp code uses
            // it both for a native array (`nqp::create(array[uint32])`) and to
            // hand-build an iterator (`nqp::create(self)` followed by
            // `bindattr`), so it must not go anywhere near `new`.
            "create" => {
                let ty = args.first().cloned().unwrap_or(Value::NIL);
                // A native array / Buf / Blob is allocated with its REPR's
                // empty storage, which for mutsu means `.new`: `CREATE` hands
                // back a value with no element storage at all, so the
                // `nqp::push_i` that invariably follows
                // (`nqp::strtocodes($s, NFC, nqp::create(array[uint32]))`)
                // had nothing to push onto. Everything else takes `CREATE`,
                // whose whole point here is to skip the constructor.
                let name = match ty.view() {
                    ValueView::Package(sym) => sym.resolve(),
                    _ => crate::runtime::utils::value_type_name(&ty).to_string(),
                };
                // `nqp::create(Uni)` (and the NFC/NFD/NFKC/NFKD forms) must
                // hand back an EMPTY codepoint store, which nqp code then
                // fills with `nqp::push_i` / `nqp::strtocodes`. `CREATE` would
                // answer with a bare type object instead, since a Uni's
                // content is not a Raku attribute.
                if matches!(name.as_str(), "Uni" | "NFC" | "NFD" | "NFKC" | "NFKD") {
                    let form = if name == "Uni" { String::new() } else { name };
                    return Some(Ok(Value::uni_from_codepoints(form, std::iter::empty())));
                }
                // A bare VM storage class (`is repr('VMArray')` /
                // `is repr('VMHash')`) has no attributes at all — its whole
                // content is the store — so `CREATE` would hand back something
                // `nqp::bindpos`/`nqp::bindkey` cannot write to. Allocate
                // mutsu's own array/hash, which IS that store.
                {
                    let reg = self.registry();
                    let short = name.rsplit("::").next().unwrap_or(&name);
                    let holds = |set: &rustc_hash::FxHashSet<String>| {
                        set.contains(&name)
                            || set
                                .iter()
                                .any(|c| c.rsplit("::").next().unwrap_or(c) == short)
                    };
                    if holds(&reg.vmhash_classes) {
                        return Some(Ok(Value::hash_with_data(Value::hash_arc(
                            ValueMap::default(),
                        ))));
                    }
                    if holds(&reg.vmarray_classes) {
                        return Some(Ok(Value::real_array(Vec::new())));
                    }
                }
                // A Map/Hash/List/Array is, in mutsu, indistinguishable from
                // its own storage — the identity `nqp_attr_value` already
                // answers a `'$!storage'` read with — so `CREATE` has to
                // allocate that storage, because nqp code creates one of these
                // and then builds it with `nqp::bindkey` / `nqp::push`, or
                // installs a separately built store into it. `Mu.CREATE` hands
                // back an attribute-less instance, which none of those reach;
                // `.new` with no arguments is the empty store, correctly
                // tagged (a `Map` is a Hash flagged immutable, a `List` an
                // Array flagged immutable).
                let method = if name.starts_with("array[")
                    || name == "array"
                    || matches!(name.as_str(), "Map" | "Hash" | "List" | "Array")
                    || crate::runtime::utils::is_buf_or_blob_class(&name)
                {
                    "new"
                } else {
                    "CREATE"
                };
                // `nqp::create` is the REPR-level allocation: it never runs a
                // user `CREATE` method (rakudo prints nothing for a class whose
                // `method CREATE` says something). Allocate directly rather
                // than through `call_method_with_values`, whose resolution
                // walk before its own `CREATE` arm cost ~20K instructions a
                // call (#9122).
                if method == "CREATE"
                    && let Some(result) = self.dispatch_create(&ty)
                {
                    return Some(result);
                }
                Ok(match self.call_method_with_values(ty, method, vec![]) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                })
            }

            // nqp::getattr($obj, $class, '$!name') and the typed reads —
            // straight attribute access, with the class operand ignored
            // because mutsu stores one flat attribute map per instance rather
            // than a per-class slot table. (A private attribute of the same
            // name in two classes of one hierarchy would therefore collide;
            // that is the same limitation `$!name` access already has.)
            "getattr" | "getattr_i" | "getattr_n" | "getattr_s" => {
                let obj = args.first().cloned().unwrap_or(Value::NIL);
                let name = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let value = Self::nqp_attr_value(&obj, &name);
                Ok(match op {
                    "getattr_i" => Value::int(value.as_ref().map(to_int).unwrap_or(0)),
                    "getattr_n" => Value::num(value.as_ref().map(|v| v.to_f64()).unwrap_or(0.0)),
                    "getattr_s" => Value::str(
                        value
                            .as_ref()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    ),
                    _ => value.unwrap_or(Value::NIL),
                })
            }
            // nqp::setelems($buf, $n): resize a buffer to `$n` elements, the
            // extra ones zero. `NativeHelpers::Blob`'s `blob-allocate` is
            // `blob.new` followed by this, so a `Buf` out-parameter of a native
            // call is allocated through it.
            "setelems" => {
                let target = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                let n = args.get(1).map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                    && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                {
                    let resized = crate::value::value_buf::with_buf_elems_mut(&attributes, |e| {
                        e.resize(n, Value::int(0))
                    });
                    if resized.is_none() {
                        // A `Blob` type object carries no storage at all; give
                        // it some rather than silently answering an empty one.
                        let mut map = attributes.to_map();
                        crate::value::value_buf::set_buf_elems(
                            &mut map,
                            class_name,
                            vec![Value::int(0); n],
                        );
                        attributes.commit_attrs(map);
                    }
                    return Some(Ok(target.clone()));
                }
                // A plain (or native typed) array target: resize its items in
                // place — CBOR::Simple presizes `array[num32].new` this way.
                if let ValueView::Array(items, _) = target.view() {
                    // SAFETY: audited aliased in-place container write (see
                    // value::aliased_mut); no borrow into the node is live.
                    let data = unsafe { crate::value::gc_contents_mut(&items) };
                    data.items_mut().resize(n, Value::int(0));
                    return Some(Ok(target.clone()));
                }
                Ok(target)
            }
            _ => return None,
        })
    }

    /// Write one attribute cell directly, bypassing accessors — the shared
    /// body of `nqp::bindattr` and `nqp::p6bindattrinvres`, which differ only
    /// in what they hand back.
    ///
    /// A container target is the interesting case: rakudo's List and Map keep
    /// their elements in a separate storage object that nqp code installs
    /// through `'$!reified'` / `'$!storage'`, and mutsu has no such wrapper —
    /// see [`Interpreter::nqp_bind_container_storage`], which unifies the two
    /// stores instead.
    pub(crate) fn nqp_bindattr_value(
        op: &str,
        obj: &Value,
        attr: &str,
        val: Value,
    ) -> Result<(), RuntimeError> {
        let attr_key = attr
            .trim_start_matches(['$', '@', '%', '&'])
            .trim_start_matches(['!', '.']);
        if attr_key.is_empty() {
            return Err(RuntimeError::new(format!(
                "nqp::{op}: empty attribute name"
            )));
        }
        if Self::nqp_bind_container_storage(attr_key, obj, &val) {
            return Ok(());
        }
        if let ValueView::Instance { attributes, .. } = obj.view() {
            let mut updated = attributes.to_map();
            updated.insert_through(attr_key, val);
            attributes.commit_attrs(updated);
        }
        Ok(())
    }

    /// The attribute-map key for an nqp `'$!name'` operand. nqp always spells
    /// the twigil; mutsu's instance maps are keyed by the bare name, so try
    /// both rather than assuming one (an attribute declared `@!items` is
    /// asked for as `'@!items'` and stored as `items`).
    fn nqp_attr_keys(name: &str) -> Vec<String> {
        let bare = name
            .strip_prefix("$!")
            .or_else(|| name.strip_prefix("@!"))
            .or_else(|| name.strip_prefix("%!"))
            .or_else(|| name.strip_prefix("&!"))
            .unwrap_or(name);
        if bare == name {
            vec![name.to_string()]
        } else {
            vec![bare.to_string(), name.to_string()]
        }
    }

    fn nqp_attr_value(obj: &Value, name: &str) -> Option<Value> {
        // `nqp::getattr($map, Map, '$!storage')` reaches for the hash a Map
        // wraps, so that nqp's bindkey/deletekey can build it in place. In
        // mutsu a Map/Hash IS that storage — there is no wrapper object with a
        // separate `$!storage` slot — so the hash answers for itself, and the
        // in-place mutations the caller then performs land on the same `Gc`
        // the Map value holds.
        if let ValueView::Hash(_) = obj.view() {
            return Some(obj.clone());
        }
        // Rakudo represents a slurpy List's reified storage as the value of
        // `$!reified`.  mutsu stores List/Array elements directly in the
        // Array value, so expose that same backing value to nqp code such as
        // Array::Sorted::Util's `insert-also`.
        if matches!(obj.view(), ValueView::Array(..))
            && Self::nqp_attr_keys(name)
                .iter()
                .any(|key| matches!(key.as_str(), "reified" | "storage"))
        {
            return Some(obj.clone());
        }
        // A `Match`'s NQP-level attribute names are not the keys mutsu stores,
        // and a still-lazy Match is not an `Instance` at all, so neither the
        // bare-name nor the twigil spelling would find them. rakudo's `$!pos`
        // is mutsu's `to` -- the position the match (or cursor) reached, which
        // `.pos` already reads -- and there is no separate `$!to`. This is what
        // lets the cursor protocol's `nqp::getattr_i($cursor, Match, '$!pos')`
        // read where a hand-driven regex got to (#7883).
        if obj.is_match_instance() {
            let bare = Self::nqp_attr_keys(name).swap_remove(0);
            return match bare.as_str() {
                "pos" | "to" => obj.match_to().map(Value::int),
                "from" => obj.match_from().map(Value::int),
                "orig" => obj.match_orig(),
                "made" | "ast" => Some(obj.match_ast().unwrap_or(Value::NIL)),
                _ => None,
            };
        }
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return None;
        };
        let attrs = attributes.to_map();
        Self::nqp_attr_keys(name)
            .into_iter()
            .find_map(|k| attrs.get(&k).cloned())
    }
}
