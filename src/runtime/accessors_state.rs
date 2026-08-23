//! Callable signature/composition and interpreter state accessors:
//! `our`/`state`/once vars, wrap chains, method/multi/proto dispatch frames.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn callable_signature(&self, callable: &Value) -> (Vec<String>, Vec<ParamDef>) {
        match callable.view() {
            ValueView::Sub(data) => (data.params.clone(), data.param_defs.clone()),
            ValueView::Routine { name, .. } => {
                if let Some(def) = self.resolve_function(&name.resolve()) {
                    return (def.params.clone(), def.param_defs.clone());
                }
                if let Some(def) = self.resolve_proto_function(&name.resolve()) {
                    return (def.params.clone(), def.param_defs.clone());
                }
                if let Some(arity) = Self::inferred_operator_arity(&name.resolve()) {
                    let params = (0..arity).map(|i| format!("arg{}", i)).collect();
                    return (params, Vec::new());
                }
                // Well-known 0-arity terms, and slurpy (`*@args`) builtins whose
                // required-positional arity is 0, should report no parameters
                // (raku: `&warn.arity == 0`). Mustache's logger keys on
                // `&warn.?arity == 2`, so a spurious arity of 1 sent it down the
                // wrong (2-arg) branch.
                if matches!(
                    name.resolve().as_ref(),
                    "rand"
                        | "now"
                        | "time"
                        | "warn"
                        | "note"
                        | "say"
                        | "print"
                        | "put"
                        | "die"
                        | "fail"
                ) {
                    return (Vec::new(), Vec::new());
                }
                (vec!["arg0".to_string()], Vec::new())
            }
            _ => (vec!["arg0".to_string()], Vec::new()),
        }
    }

    pub(crate) fn infix_associativity(&self, full_name: &str) -> Option<String> {
        let fq = format!("{}::{}", self.current_package(), full_name);
        self.operator_assoc
            .get(&fq)
            .cloned()
            .or_else(|| self.operator_assoc.get(full_name).cloned())
            .or_else(|| {
                let global = format!("GLOBAL::{}", full_name);
                self.operator_assoc.get(&global).cloned()
            })
    }

    pub(crate) fn call_user_routine_direct(
        &mut self,
        full_name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(def) = self.resolve_function_with_alias(full_name, &args) {
            return self.call_routine_def(&def, args);
        }
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }
        let env_name = format!("&{}", full_name);
        if let Some(callable) = self.env.get(&env_name).cloned() {
            return self.eval_call_on_value(callable, args);
        }
        // Try stripping package prefix (e.g., "Main::foo" -> "foo")
        // when the function is in the current or GLOBAL package.
        if let Some(pos) = full_name.rfind("::") {
            let short_name = &full_name[pos + 2..];
            if let Some(def) = self.resolve_function_with_alias(short_name, &args) {
                return self.call_routine_def(&def, args);
            }
            let env_short = format!("&{}", short_name);
            if let Some(callable) = self.env.get(&env_short).cloned() {
                return self.eval_call_on_value(callable, args);
            }
        }
        let suggestions = self.suggest_routine_names(full_name);
        Err(RuntimeError::undeclared_routine_symbols(
            full_name,
            format!("Unknown function: {}", full_name),
            suggestions,
        ))
    }

    pub(crate) fn compose_callables(&self, left: Value, right: Value) -> Value {
        use std::sync::atomic::{AtomicU64, Ordering};

        static COMPOSE_ID: AtomicU64 = AtomicU64::new(1_000_000);
        let id = COMPOSE_ID.fetch_add(1, Ordering::Relaxed);

        let (mut params, param_defs) = self.callable_signature(&right);
        if params.is_empty() {
            if !param_defs.is_empty() {
                params = param_defs.iter().map(|pd| pd.name.clone()).collect();
            } else {
                params = vec!["arg0".to_string()];
            }
        }

        let left_return_type = self.callable_return_type(&left);
        let mut env = crate::env::Env::new();
        env.insert("__mutsu_compose_left".to_string(), left);
        env.insert("__mutsu_compose_right".to_string(), right);
        if let Some(rt) = left_return_type {
            env.insert("__mutsu_return_type".to_string(), Value::str(rt));
        }

        Value::make_sub_with_id(
            Symbol::intern(""),
            Symbol::intern("<composed>"),
            params,
            param_defs,
            Vec::new(),
            false,
            env,
            id,
        )
    }

    pub(crate) fn env_mut(&mut self) -> &mut Env {
        &mut self.env
    }

    /// Recursively collect every `needs_cell_escaping_our_sub` name from `code` and
    /// its nested closure codes into `escaping_our_lexical_names`. Called once at
    /// program start so the set is populated before any block runs.
    pub(crate) fn collect_escaping_our_lexical_names(
        &mut self,
        code: &crate::opcode::CompiledCode,
    ) {
        for sym in &code.needs_cell_escaping_our_sub {
            self.escaping_our_lexical_names.insert(sym.resolve());
        }
        for nested in &code.closure_compiled_codes {
            self.collect_escaping_our_lexical_names(nested);
        }
    }

    /// Resolve a free-variable read of a block lexical captured by an `our`-scoped
    /// named sub (`escaping_our_lexical_names`). Returns:
    ///   * `Some(cell)` — the persisted shared cell, once the declaring block has
    ///     run and recorded it (`escaped_our_lexical_cells`).
    ///   * `Some(Nil)`  — when the name IS such a capture but no cell is recorded
    ///     yet (a call BEFORE the block runs). This deliberately SHORT-CIRCUITS the
    ///     normal env lookup so an unrelated leaked `env` value from a sibling block
    ///     cannot shadow the (still-undefined) captured lexical.
    ///   * `None`       — not an escaping-our capture, or read outside any routine
    ///     (a bare top-level reference must not see the closure's private lexical):
    ///     the caller resolves normally.
    pub(crate) fn escaping_our_read(&self, name: &str) -> Option<Value> {
        if self.escaping_our_lexical_names.is_empty()
            || self.routine_stack.is_empty()
            || name.contains("::")
            || !self.escaping_our_lexical_names.contains(name)
            || !self.in_escaped_our_sub()
        {
            return None;
        }
        Some(
            self.escaped_our_lexical_cells
                .get(name)
                .cloned()
                .unwrap_or(Value::NIL),
        )
    }

    /// Whether the innermost NAMED routine frame is one of the `our`-scoped
    /// subs recorded by `RegisterSub` (`escaped_our_sub_names`). Anonymous
    /// block/closure frames (`<pointy-block>`, `<anon>`, empty names) are
    /// transparent — a nested `if`/bare block inside the sub still counts.
    /// Gates the escaped-cell resolution so a plain `my sub` that merely
    /// shares a captured variable's name — called while its declaring block is
    /// still live — keeps resolving through its own env capture instead of the
    /// escaped sub's persisted cell. Note: an escaped sub reached through a
    /// code var (`&OUR::f()`) is pushed as a *block* frame with its real name,
    /// so the walk keys on the NAME, not `is_block`.
    fn in_escaped_our_sub(&self) -> bool {
        for frame in self.routine_stack.iter().rev() {
            let name = frame.name.as_str();
            if self.escaped_our_sub_names.contains(name)
                || name
                    .rsplit("::")
                    .next()
                    .is_some_and(|bare| self.escaped_our_sub_names.contains(bare))
            {
                return true;
            }
            // A real (named) routine that is NOT an escaped our sub: its own
            // captures win — stop walking.
            if !name.is_empty() && !name.starts_with('<') {
                return false;
            }
        }
        false
    }

    /// Resolve a bare-name WRITE (`$a = v`, `$a++`, `$a += v`) of a block
    /// lexical captured by an escaped `our` sub through its persisted shared
    /// cell. Consulted FIRST in the write chains, mirroring the read side
    /// (`escaping_our_read` short-circuits the env lookup): the shared env may
    /// hold a stale leaked value for the name (e.g. `sync_env_from_locals`
    /// flushing the declaring block's dead top-level slot as Nil), which would
    /// otherwise absorb the write into a plain env copy the next read (which
    /// resolves through the cell) never sees.
    ///
    /// Returns the `ContainerRef` cell only when one is actually recorded (and
    /// `name` is neither a local nor an upvalue of `code`), so unrelated
    /// same-named locals/captures and pre-block calls stay untouched. A plain
    /// `my sub` sharing the variable name is called while its declaring block
    /// is live, so its captured `$a` is an upvalue/local and is excluded here.
    pub(crate) fn escaping_our_write_cell(
        &self,
        code: &crate::opcode::CompiledCode,
        name: &str,
    ) -> Option<Value> {
        if self.escaping_our_lexical_names.is_empty() {
            return None;
        }
        if code.locals.iter().any(|n| n == name)
            || code.upvalue_syms.iter().any(|s| s.with_str(|u| u == name))
        {
            return None;
        }
        // Only intercept when a real persisted cell exists (a `ContainerRef`);
        // `escaping_our_read` otherwise yields `Nil` (pre-block call), which must
        // fall through to the normal env chain untouched.
        self.escaping_our_read(name)
            .filter(|v| v.is_container_ref())
    }

    /// Get a cloned copy of the persisted closure env for a given closure id.
    pub(crate) fn get_closure_env_override(&self, id: u64) -> Option<crate::env::Env> {
        self.closure_env_overrides.get(&id).cloned()
    }

    /// Check whether a sub has an active (non-empty) wrap chain.
    pub(crate) fn has_wrap_chain(&self, sub_id: u64) -> bool {
        self.wrap_chains.get(&sub_id).is_some_and(|c| !c.is_empty())
    }

    /// Check whether we're already inside a wrap dispatch for a given sub.
    pub(crate) fn is_wrap_dispatching(&self, sub_id: u64) -> bool {
        self.wrap_dispatch_stack.iter().any(|f| f.sub_id == sub_id)
    }

    /// Find the sub_id and Sub value for a function name that has an active wrap chain.
    /// Returns the sub_id if a wrap chain exists for the given function name.
    pub(crate) fn wrap_sub_id_for_name(&self, name: &str) -> Option<u64> {
        for (sub_id, sub_name) in &self.wrap_sub_names {
            if sub_name == name && self.has_wrap_chain(*sub_id) {
                return Some(*sub_id);
            }
        }
        None
    }

    /// Get the original wrapped Sub value for a function name.
    /// Returns the Sub value stored when wrap was called, preserving the original sub_id.
    pub(crate) fn get_wrapped_sub(&self, name: &str) -> Option<Value> {
        self.wrap_name_to_sub.get(name).cloned()
    }

    pub(crate) fn get_our_var(&self, key: &str) -> Option<&Value> {
        self.our_vars.get(key)
    }

    /// Mutable counterpart of [`Self::get_our_var`], for the container
    /// write chokepoint (`env_root_descended_mut`): a package's `our @a`/`our
    /// %h` is mutated in place through its stored `Gc`, not replaced.
    pub(crate) fn get_our_var_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.our_vars.get_mut(key)
    }

    pub(crate) fn our_vars_iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.our_vars.iter()
    }

    pub(crate) fn our_vars_is_empty(&self) -> bool {
        self.our_vars.is_empty()
    }

    pub(crate) fn set_our_var(&mut self, key: String, value: Value) {
        self.our_vars.insert(key, value);
    }

    pub(crate) fn get_state_var(&self, key: (Symbol, Option<u64>)) -> Option<&Value> {
        self.state_vars.get(&key)
    }

    /// Drop a `state` variable's stored value so the next `StateVarInit`
    /// re-runs its initializer. Used when a loop statement is re-entered:
    /// each execution of the statement is a fresh clone of its body block
    /// (Raku clones a block each time its enclosing block runs), so `state`
    /// declarations inside the body re-initialize.
    ///
    /// The cross-thread cell (`get_or_init_shared_state_cell`) must go too.
    /// Once any thread has been spawned, `StateVarInit` resolves the variable
    /// through that cell and ignores the local store entirely, so dropping only
    /// the local entry left the reset a silent no-op: a loop-body `state` kept
    /// counting up across later executions of its enclosing statement — Cro's
    /// `Cro.compose` recursed on a `state $split` that never restarted at 1 and
    /// blew the stack once a `Cro::Service.start` had run.
    pub(crate) fn remove_state_var(&mut self, key: (Symbol, Option<u64>)) {
        self.state_vars.remove(&key);
        self.shared_vars.remove(&Self::shared_state_cell_key(key));
    }

    /// Reconstruct the pre-rekey display string for a `(Symbol, Option<u64>)`
    /// state key — `{base}#c{id}` when scoped to a closure clone, else the bare
    /// base key. Only the cross-thread cell-key derivation
    /// (`normalize_state_key`) still needs a string; the store itself and every
    /// hot-path read/write use the Copy tuple directly.
    pub(crate) fn state_key_display(key: (Symbol, Option<u64>)) -> String {
        match key.1 {
            Some(id) => format!("{}#c{}", key.0.as_str(), id),
            None => key.0.as_str().to_string(),
        }
    }

    /// Reverse of `state_key_display`. Used only by the `__mutsu_state_key::*`
    /// env metadata bridge (a closure's free-var state writeback path, see
    /// `vm_closure_dispatch.rs`), which persists the key as a string `Value`
    /// since `Env` is string-keyed.
    pub(crate) fn state_key_from_display(s: &str) -> (Symbol, Option<u64>) {
        if let Some(pos) = s.rfind("#c")
            && !s[pos + 2..].is_empty()
            && s[pos + 2..].bytes().all(|b| b.is_ascii_digit())
        {
            let id: u64 = s[pos + 2..].parse().unwrap_or(0);
            (Symbol::intern(&s[..pos]), Some(id))
        } else {
            (Symbol::intern(s), None)
        }
    }

    /// The `shared_vars` key under which `key`'s cross-thread `state` cell lives.
    pub(crate) fn shared_state_cell_key(scoped_key: (Symbol, Option<u64>)) -> String {
        format!(
            "__mutsu_shared_state::{}",
            crate::runtime::Interpreter::normalize_state_key(&Self::state_key_display(scoped_key))
        )
    }

    pub(crate) fn set_state_var(&mut self, key: (Symbol, Option<u64>), value: Value) {
        // Track C/Track B: once a `state` variable lives in a shared
        // `ContainerRef` cell (StateVarInit under an active thread context),
        // every writeback must go THROUGH the cell, not replace the store
        // entry with a plain snapshot. The block-exit sync
        // (`sync_state_locals`) hands us the mutated plain aggregate; before
        // this write-through, storing it here severed the cell, so the next
        // call's StateVarInit re-read the stale cell content and `state @a` /
        // `state %h` accumulation was lost entirely whenever a thread had
        // ever been spawned (deterministic: `%h<k>++` returned 1,1,1... —
        // t/state-aggregate-shared-cell.t). Writing a cell over a cell (the
        // StateVarInit path itself) keeps the plain insert.
        if let Some(ValueView::ContainerRef(cell)) = self.state_vars.get(&key).map(Value::view)
            && !value.is_container_ref()
        {
            *cell.lock().unwrap_or_else(|e| e.into_inner()) = value;
            return;
        }
        self.state_vars.insert(key, value);
    }

    /// Track C: get-or-create a shared `ContainerRef` cell for a `state` variable
    /// in `shared_vars` (the cross-thread store), keyed by `key`. Used while a
    /// thread is running so that concurrent calls to the same routine — e.g.
    /// `await (^3).map: { start f() }` where `f` has `state $n` — share one live
    /// cell instead of each thread initializing its own snapshot. The get-or-init
    /// is atomic under the `shared_vars` write lock, so the first caller seeds the
    /// cell (from `initial`) and the rest observe it. Returns the cell value.
    pub(crate) fn get_or_init_shared_state_cell(&self, key: &str, initial: Value) -> Value {
        self.shared_vars.get_or_init_cell(key, || {
            // Track B slice 3: aggregates are celled at StateVarInit in every mode,
            // so the pre-thread seed may already BE a cell — adopt it rather than
            // double-wrapping (a cell inside a cell would break every deref path).
            if initial.is_container_ref() {
                initial
            } else {
                initial.into_container_ref()
            }
        })
    }

    /// Read per-closure-instance captured-variable state (hot closure-call path).
    pub(crate) fn get_closure_captured_state(&self, id: u64, name: Symbol) -> Option<&Value> {
        self.closure_captured_state.get(&(id, name))
    }

    /// Persist per-closure-instance captured-variable state (hot closure-call path).
    pub(crate) fn set_closure_captured_state(&mut self, id: u64, name: Symbol, value: Value) {
        self.closure_captured_state.insert((id, name), value);
    }

    /// Drop all per-closure-instance captured-variable state for a closure id.
    /// Used by the react drive loop so a `whenever`/`LAST`/`QUIT` callback re-reads
    /// its captured-outer lexicals from the live caller env (which every sibling
    /// callback in the same react block writes back to) instead of restoring a
    /// stale per-instance snapshot that would clobber a sibling's update.
    /// True for the two builtin bases whose subclasses keep their elements in
    /// the instance's `__mutsu_array_storage` and delegate positional methods
    /// to it: `Array` and `List`.
    ///
    /// `class C is List { }` needs the same treatment as `class C is Array { }`
    /// — `List.new` takes its elements positionally and the instance answers
    /// `.elems`/`.join`/`.list`/`AT-POS` from them. Cro's
    /// `Cro::HTTP::MultiValue is List does Stringy` is built exactly that way,
    /// so without this a query string or form body with a repeated key could
    /// not be represented at all.
    pub(crate) fn is_positional_base(name: &str) -> bool {
        name == "Array" || name == "List"
    }

    /// The backing store a fresh `is Array`/`is List` subclass instance gets.
    /// A `List` subclass must be backed by an immutable `List`, not an `Array`,
    /// so `.push` on one still raises `X::Immutable` as raku does.
    pub(crate) fn positional_base_storage(&mut self, class_key: &str, items: Vec<Value>) -> Value {
        if self.class_mro(class_key).iter().any(|n| n == "Array") {
            Value::real_array(items)
        } else {
            Value::array(items)
        }
    }

    /// True for the two builtin bases whose subclasses keep their entries in
    /// the instance's `__mutsu_hash_storage` and delegate Associative
    /// methods to it: `Hash` and `Map`. Mirrors [`Self::is_positional_base`].
    ///
    /// `class C is Hash { }` (and, less commonly, `class C is Map { }`) needs
    /// the same treatment `is Array`/`is List` subclasses already get: the
    /// instance's key/value data lives in the backing storage and every
    /// Associative-protocol method (`AT-KEY`, `keys`, `.raku`, ...) answers
    /// from it instead of the generic attribute bag.
    pub(crate) fn is_associative_base(name: &str) -> bool {
        name == "Hash" || name == "Map"
    }

    /// The backing store a fresh `is Hash`/`is Map` subclass instance gets,
    /// seeded from `pairs` (constructor `Pair` args that do not name a
    /// declared attribute). A `Map` subclass that is not ALSO an `is Hash`
    /// subclass (`Hash` extends `Map`, so its own MRO always contains both)
    /// is backed by an immutable Map — `%m<a> = 1` on it must still raise
    /// like raku's `X::Assignment::RO`, matching how
    /// [`Self::positional_base_storage`] picks an immutable `List` over a
    /// mutable `Array` for an `is List`-but-not-`is Array` subclass.
    pub(crate) fn associative_base_storage(&mut self, class_key: &str, pairs: Vec<Value>) -> Value {
        let mut map = HashMap::new();
        for item in pairs {
            match item.view() {
                ValueView::Pair(k, v) => {
                    map.insert(k.to_string(), v.clone());
                }
                ValueView::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), v.clone());
                }
                _ => {}
            }
        }
        let result = Value::hash(map);
        if self.class_mro(class_key).iter().any(|n| n == "Hash") {
            result
        } else {
            self.tag_container_metadata(
                result,
                super::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                },
            )
        }
    }

    pub(crate) fn clear_closure_captured_state_for(&mut self, id: u64) {
        self.closure_captured_state
            .retain(|(entry_id, _), _| *entry_id != id);
    }

    /// Build the shared-store key for a `once` site at bytecode position
    /// `op_ip`. The key combines the enclosing code-object *clone* identity with
    /// the op position, so a `once` fires once per clone (Raku semantics):
    ///
    /// - Inside a **method**, the clone is identified by `(owning class, method)`
    ///   — a stable, per-clone key (unlike the method's per-*call*
    ///   `__mutsu_callable_id`, which is a fresh id every invocation). A method
    ///   fires once per clone: once total for a normal/inherited method (the
    ///   defining class's single clone), and once per composing class for a role
    ///   method (`owner_class` is the composing class there). The innermost
    ///   routine frame is consulted, so a `once` in a closure/sub *nested* in a
    ///   method still keys on that inner clone, not the method's.
    /// - Otherwise `__mutsu_callable_id` (set per-clone by the sub/closure call
    ///   dispatch) identifies a specific routine/closure clone — this makes a
    ///   fresh `my sub` clone per loop iteration re-fire, and (being stable across
    ///   a routine's OTF recompiles on worker threads) makes a sub's `once` agree
    ///   across threads.
    /// - Otherwise the innermost block/once scope (top-level / bare-block `once`).
    ///
    /// The `m`/`c`/`r` tag keeps the id spaces from colliding numerically, and
    /// `op_ip` is unique per `once` site within a single code object, so the key
    /// is collision-free. Unlike the old global compile-time counter it is
    /// deterministic across every recompilation of the same source.
    pub(crate) fn once_scope_key(&self, op_ip: usize) -> String {
        if let Some(frame) = self.routine_stack.last()
            && frame.is_method
        {
            return format!("m{}::{}::{op_ip}", frame.package, frame.name);
        }
        match self.env.get("__mutsu_callable_id").map(Value::view) {
            Some(ValueView::Int(id)) if id >= 0 => format!("c{id}::{op_ip}"),
            _ => format!(
                "r{}::{op_ip}",
                self.once_scope_stack.last().copied().unwrap_or(0)
            ),
        }
    }

    /// The shared cross-thread `once` result store (see [`once_scope_key`]).
    pub(crate) fn once_store(&self) -> &std::sync::Arc<crate::runtime::once_store::OnceStore> {
        &self.once_values
    }

    pub(crate) fn push_once_scope(&mut self, scope: u64) {
        self.once_scope_stack.push(scope);
    }

    pub(crate) fn pop_once_scope(&mut self) {
        self.once_scope_stack.pop();
    }

    pub(crate) fn next_once_scope_id(&mut self) -> u64 {
        let scope = self.next_once_scope_id;
        self.next_once_scope_id += 1;
        scope
    }

    pub(crate) fn when_matched(&self) -> bool {
        self.when_matched.get()
    }

    pub(crate) fn set_when_matched(&self, v: bool) {
        self.when_matched.set(v);
    }

    pub(crate) fn is_role(&self, name: &str) -> bool {
        self.registry().roles.contains_key(name)
    }

    pub(crate) fn push_method_class(&mut self, class_name: String) {
        self.method_class_stack.push(class_name);
    }

    pub(crate) fn pop_method_class(&mut self) {
        self.method_class_stack.pop();
    }

    /// The package a closure created right now is lexically inside. A closure
    /// declared in a method of `C` is still lexically inside `C` — `self!priv`
    /// from its body is an in-class call, and it must stay legal when the Sub is
    /// invoked later from a foreign frame. `current_package` is only switched to
    /// the class for some method shapes (class-scoped subs, package lexicals, a
    /// `::`-qualified owner), so fall back to the running method's class.
    pub(crate) fn lexical_closure_package(&self) -> String {
        // A closure created inside a METHOD body lexically belongs to that
        // method's class, even when `current_package` still holds the CALLER's
        // package (method dispatch pushes `method_class_stack` but does not
        // re-point `current_package`). Walk past block frames to the innermost
        // routine: if it is a method, its class wins — `start Transform.new`
        // inside TestConnector.connect must capture TestConnector, not the
        // Cro::CompositeConnector frame that invoked it (nested-class short
        // names resolve against the captured package). An innermost SUB frame
        // keeps the package rules (a module sub called from a method must not
        // inherit the method's class).
        for f in self.routine_stack.iter().rev() {
            if f.is_block {
                continue;
            }
            if f.is_method
                && let Some(class) = self.method_class_stack.last()
            {
                return class.clone();
            }
            break;
        }
        let pkg = self.current_package();
        if (pkg.is_empty() || pkg == "GLOBAL")
            && let Some(class) = self.method_class_stack.last()
        {
            return class.clone();
        }
        pkg
    }

    pub(crate) fn method_class_stack_top(&self) -> Option<String> {
        self.method_class_stack.last().cloned()
    }

    /// Borrowing form of [`Self::method_class_stack_top`]. The attribute cell-key
    /// resolution consults this on every `$!x` read, where the owned clone was a
    /// per-access heap allocation.
    pub(crate) fn method_class_stack_top_str(&self) -> Option<&str> {
        self.method_class_stack.last().map(String::as_str)
    }

    /// Set up a method dispatch frame for nextsame/callsame support.
    /// Returns true if a frame was pushed (caller must call pop_method_dispatch).
    /// Also pushes a samewith context unconditionally for samewith() support.
    /// True if `(class, method)` has >= 2 *structural* dispatch candidates across
    /// the MRO — i.e. enough overloads that `push_method_dispatch_frame` might build
    /// a deferral (`nextsame`/`callsame`) frame. Counts the same defs
    /// `resolve_all_methods_with_owner` iterates (non-private; submethods only at
    /// the receiver level) but BEFORE arg-matching and without cloning, so the
    /// result depends only on the registry shape and is memoized in
    /// `dispatch_multi_candidate`. A `false` answer lets the caller skip the full
    /// per-call resolve: arg-matching only reduces the count, so <=1 structural
    /// candidate can never yield >=2 matched candidates (no frame is ever pushed).
    pub(crate) fn has_multiple_dispatch_candidates(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> bool {
        // ADR-0019 E3 (design decision 5, `todo/deep/adr0019-e2-e4-resolver-core.md`):
        // `dispatch_multi_candidate` is generation-blind — it relied entirely
        // on the manual clear blocks, not on `method_generation`.
        self.refresh_method_caches_for_generation();
        let class_sym = crate::symbol::Symbol::intern(class_name);
        let method_sym = crate::symbol::Symbol::intern(method_name);
        if let Some(&c) = self.dispatch_multi_candidate.get(&(class_sym, method_sym)) {
            return c;
        }
        let mro = self.class_mro(class_name);
        let mut count = 0usize;
        'outer: for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            let registry = self.registry();
            let overloads =
                registry.get_method_overloads_with_role_fallback(cn.as_str(), method_name);
            if let Some(overloads) = overloads {
                for def in &overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited (mirrors resolve_all_methods_with_owner).
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    count += 1;
                    if count >= 2 {
                        break 'outer;
                    }
                }
            }
        }
        let multi = count >= 2;
        self.dispatch_multi_candidate
            .insert((class_sym, method_sym), multi);
        multi
    }

    /// Structural fingerprint of a method body, memoized by its body-`Arc`
    /// pointer (see `method_body_fp_cache`). Use this instead of calling
    /// `function_body_fingerprint` directly on the method-redispatch hot path
    /// (`nextsame`/`samewith`/multi-method deferral): the raw call
    /// Debug-traverses the entire body AST every time, which a perf profile of a
    /// samewith-tight loop showed dominating the redispatch cost.
    pub(crate) fn method_def_fingerprint(&mut self, def: &MethodDef) -> u64 {
        let key = Arc::as_ptr(&def.body) as usize;
        if let Some((_, fp)) = self.method_body_fp_cache.get(&key) {
            return *fp;
        }
        let fp = crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
        self.method_body_fp_cache
            .insert(key, (def.body.clone(), fp));
        fp
    }

    /// Lazily clear `func_multi_resolve_cache`/`func_multi_type_cacheable` when
    /// `fn_resolve_gen` has advanced since they were last built — the function-dispatch
    /// analogue of `refresh_method_caches_for_generation`. ADR-0019 Phase F box F5: these
    /// two caches used to depend entirely on the eager clear in
    /// `invalidate_method_dispatch_caches`, which covered only 7 call sites even though
    /// `fn_resolve_gen` itself is bumped at ~15 other sub/multi-registration sites
    /// (`registration_sub.rs`, `methods_sub.rs`, module load/import, ...) that never called
    /// it — a real staleness gap, not just duplicated cleanup, since those sites can add a
    /// new multi-sub candidate that a cached resolution would then silently skip.
    pub(crate) fn refresh_func_multi_caches_for_generation(&mut self) {
        if self.func_multi_cache_generation == self.fn_resolve_gen {
            return;
        }
        self.func_multi_cache_generation = self.fn_resolve_gen;
        self.func_multi_resolve_cache.clear();
        self.func_multi_type_cacheable.clear();
    }

    /// Whether a multi *sub* `name` (in `pkg`) has a dispatch that is purely
    /// type+arity based — the function analogue of `multi_dispatch_type_cacheable`.
    /// False when any candidate is value-/identity-dependent (`where` / literal /
    /// subset / `:D`/`:U` smiley / coercion), so its winner can't be keyed on the
    /// positional arg types alone. Memoized per `(package, name)`.
    pub(crate) fn func_multi_dispatch_type_cacheable(
        &mut self,
        pkg_sym: Symbol,
        name_sym: Symbol,
        name: &str,
    ) -> bool {
        self.refresh_func_multi_caches_for_generation();
        if let Some(&c) = self.func_multi_type_cacheable.get(&(pkg_sym, name_sym)) {
            return c;
        }
        let candidates = self.resolve_all_multi_candidates(name);
        let mut value_dependent = false;
        'outer: for def in &candidates {
            for pd in &def.param_defs {
                if pd.where_constraint.is_some() || pd.literal_value.is_some() {
                    value_dependent = true;
                    break 'outer;
                }
                // A code-signature callback param (`&cb:(Int)`) or a capture
                // subsignature (`|c($a, $b)`) dispatches on the argument's
                // *signature/shape*, not its `value_type_name` (a callback is
                // always "Sub"), so type-keying would mis-route it.
                if pd.code_signature.is_some() || pd.sub_signature.is_some() {
                    value_dependent = true;
                    break 'outer;
                }
                // An `is rw` candidate matches only a writable-lvalue argument —
                // a call-site property, not an arg-type one — so `f($var)` and
                // `f("lit")` need different winners under one type key.
                if pd.traits.iter().any(|t| t == "rw") {
                    value_dependent = true;
                    break 'outer;
                }
                if let Some(tc) = &pd.type_constraint {
                    // `:D`/`:U`/`:_` smiley or `Int(Str)` coercion => value/identity
                    // dependent; a subset type carries an implicit `where`. The `:`
                    // check also excludes enum-value (`E::a`) and qualified-value
                    // constraints, which refine within one value-type.
                    if tc.contains(':') || tc.contains('(') {
                        value_dependent = true;
                        break 'outer;
                    }
                    // Value-refining numeric pseudo-types: `Inf`/`NaN`/`UInt`/`-Inf`
                    // all match WITHIN a single `value_type_name` ("Num"/"Int") by
                    // inspecting the value, so type-keying would mis-route them
                    // (e.g. `multi f(NaN)` vs `multi f(Numeric)` both key as Num).
                    if matches!(tc.as_str(), "Inf" | "NaN" | "-Inf" | "UInt") {
                        value_dependent = true;
                        break 'outer;
                    }
                    let base = tc.split(['[', ' ']).next().unwrap_or(tc.as_str());
                    if self.registry().subsets.contains_key(base) {
                        value_dependent = true;
                        break 'outer;
                    }
                }
            }
        }
        let cacheable = candidates.len() >= 2 && !value_dependent;
        self.func_multi_type_cacheable
            .insert((pkg_sym, name_sym), cacheable);
        cacheable
    }

    /// Resolve a multi *sub* winner, consulting the sound multi-function
    /// resolution cache (`func_multi_resolve_cache`) for a type+arity-deterministic
    /// multi — avoiding the per-call registry walk + candidate match/rank/dedup in
    /// `resolve_function_with_types`. Falls back to a fresh resolve for un-keyable
    /// args (named/Junction/container), value-dependent multis, and AMBIGUOUS
    /// results (which must re-raise their pending dispatch error every call).
    /// Behaviorally identical to `resolve_function_with_types` for the caller.
    pub(crate) fn resolve_function_multi_cached(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        let Some(arg_keys) = self.multi_arg_type_keys(args) else {
            return self.resolve_function_with_types(name, args);
        };
        let pkg_sym = Symbol::intern(&self.current_package());
        let name_sym = Symbol::intern(name);
        if !self.func_multi_dispatch_type_cacheable(pkg_sym, name_sym, name) {
            return self.resolve_function_with_types(name, args);
        }
        let key = (pkg_sym, name_sym, arg_keys);
        if let Some(hit) = self.func_multi_resolve_cache.get(&key) {
            return hit.clone();
        }
        let resolved = self.resolve_function_with_types(name, args);
        // Ambiguity is signaled by `None` + a pending dispatch error; that must be
        // re-raised on every call, so don't cache it.
        let ambiguous = resolved.is_none() && self.pending_dispatch_error.is_some();
        if !ambiguous {
            self.func_multi_resolve_cache.insert(key, resolved.clone());
        }
        resolved
    }

    /// True when `class_name`'s MRO (or direct parents) includes a builtin
    /// metamodel class, i.e. the class is a user-defined HOW. Gated by the
    /// registry-level `has_metamodel_how_classes` flag so programs that never
    /// declare a HOW subclass pay a single bool read per method dispatch.
    pub(crate) fn is_metamodel_how_class(&self, class_name: &str) -> bool {
        let reg = self.registry();
        if !reg.has_metamodel_how_classes {
            return false;
        }
        reg.classes.get(class_name).is_some_and(|cd| {
            cd.mro
                .iter()
                .any(|c| Self::is_metamodel_class_name(c.as_str()))
                || cd.parents.iter().any(|c| Self::is_metamodel_class_name(c))
        })
    }

    /// Name of a builtin metamodel class a user HOW can inherit from.
    pub(crate) fn is_metamodel_class_name(name: &str) -> bool {
        matches!(
            name,
            "Metamodel::ClassHOW"
                | "Metamodel::GrammarHOW"
                | "Perl6::Metamodel::ClassHOW"
                | "Perl6::Metamodel::GrammarHOW"
        )
    }

    /// Push the samewith context for a method dispatch, plus a metamodel
    /// dispatch context when the receiver is a user-defined HOW class (so
    /// `callsame` can reach the native metamodel method as the last
    /// candidate). Always pair with `pop_method_samewith_context`.
    pub(crate) fn push_method_samewith_context(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Option<Value>,
    ) {
        self.push_samewith_context(method_name, invocant, Some(args.to_vec()));
        if self.is_metamodel_how_class(receiver_class) {
            self.metamodel_dispatch_stack.push((
                self.samewith_context_stack.len(),
                receiver_class.to_string(),
                method_name.to_string(),
                args.to_vec(),
            ));
        }
    }

    /// ADR-0019 E9b-0: mint the next push-order token for a wrap/method/multi
    /// dispatch frame. `dispatch_next_candidate`/`builtin_lastcall`/
    /// `builtin_nextcallee` compare tokens across all three deferral stacks and
    /// select the highest (innermost) live frame instead of a fixed
    /// wrap-then-method-then-multi search order.
    pub(crate) fn next_dispatch_token(&mut self) -> u64 {
        self.dispatch_token_counter += 1;
        self.dispatch_token_counter
    }

    /// ADR-0019 E9a/E9b-2: candidates from the deferral expansion whose
    /// signature matches this call's args (invocant-blind, per E8a finding
    /// 1). Shared first half of [`Self::push_method_dispatch_frame`]'s and
    /// [`Self::deferral_tail_entries`]'s computation, before either applies
    /// its own "skip the chosen winner" step.
    fn matched_deferral_candidates(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
    ) -> Vec<(Symbol, super::MethodDef)> {
        let role_bindings = self.registry().get_role_param_bindings(receiver_class);
        let expansion = self.resolve_deferral_expansion(receiver_class, method_name);
        let mut all_candidates: Vec<(Symbol, super::MethodDef)> = Vec::new();
        for (owner, def) in expansion {
            if self.method_args_match_for_invocant(
                receiver_class,
                &def,
                args,
                role_bindings.as_ref(),
                None,
            ) {
                all_candidates.push((owner, def));
            }
        }
        all_candidates
    }

    /// ADR-0019 E9b-2: the plain MRO-tail deferral entries following an
    /// ALREADY-KNOWN winning candidate `chosen_def` — shared by the two
    /// method-wrap entry sites (`class_dispatch.rs`'s
    /// `run_instance_method_celled`, `vm_call_method_compiled.rs`'s
    /// `check_method_wrap_chain`), which resolve their own winner
    /// independently (it is the wrapped candidate) and only need the tail to
    /// append after their own Wrapper-prefixed `Candidate{wraps_spliced:
    /// true}` entry. Every entry here is `wraps_spliced: false` — a later
    /// entry with its OWN wrap chain is spliced lazily at advance time
    /// (`dispatch_next_candidate`), not here (decision 3 of the E9b design).
    pub(crate) fn deferral_tail_entries(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        chosen_def: &super::MethodDef,
    ) -> Vec<super::DeferralEntry> {
        // Submethod fast path, mirroring `push_method_dispatch_frame`'s own
        // `<=1` guard: a submethod is never inherited, so a single visible
        // candidate can never produce a deferral tail.
        if (chosen_def.is_my || chosen_def.is_submethod)
            && self.count_visible_method_candidates(receiver_class, method_name) <= 1
        {
            return Vec::new();
        }
        let all_candidates = self.matched_deferral_candidates(receiver_class, method_name, args);
        let chosen_fp = self.method_def_fingerprint(chosen_def);
        let mut remaining = Vec::new();
        let mut skipped = false;
        for (owner, def) in all_candidates {
            let fp = self.method_def_fingerprint(&def);
            if !skipped && fp == chosen_fp {
                skipped = true;
                continue;
            }
            if self.should_skip_defer_method_candidate(receiver_class, owner.as_str()) {
                continue;
            }
            remaining.push(super::DeferralEntry::Candidate {
                owner,
                def: Box::new(def),
                wraps_spliced: false,
            });
        }
        remaining
    }

    /// ADR-0019 E9b-2: build and push the SINGLE wrap-prefixed
    /// `MethodDispatchFrame` for a method-wrap entry site — the below-
    /// outermost wrappers (in call order), the winner as a `wraps_spliced:
    /// true` `Candidate`, then the MRO tail. Shared by the two method-wrap
    /// entry sites (`class_dispatch.rs`'s `run_instance_method_celled`,
    /// `vm_call_method_compiled.rs`'s `check_method_wrap_chain`), which then
    /// invoke `chain`'s outermost wrapper (`chain.last()`) directly with
    /// `[invocant, ...args]` — mirrors the pattern every other VM call site
    /// uses `push_method_dispatch_frame` for, kept as a `pub(crate)`
    /// Interpreter method (rather than the caller touching
    /// `method_dispatch_stack` directly) so `vm/` call sites do not need
    /// visibility into private `runtime` fields/modules.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn push_wrapped_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Value,
        owner_class: Symbol,
        method_def: &super::MethodDef,
        chain: &[(u64, Value)],
    ) {
        let mro_tail = self.deferral_tail_entries(receiver_class, method_name, args, method_def);
        let rw_params =
            super::builtins_dispatch_next::rw_scalar_positional_params(&method_def.param_defs);
        let dispatch_token = self.next_dispatch_token();
        // Save the pending call-site arg sources BEFORE the outermost
        // wrapper's own binding consumes them: the `Wrapper`/wraps_spliced
        // `Candidate` advance legs in `dispatch_next_candidate` restore this
        // so an `is rw`/sigilless param anywhere in the chain still binds to
        // the true call-site variable (`t/wrap-invocant-arg-source.t`).
        let arg_sources = self.pending_call_arg_sources().cloned();
        // Below-outermost wrappers, in call order (second-outermost first,
        // innermost/oldest last) — the outermost is invoked directly by the
        // caller, mirroring today's "outermost runs, the rest are remaining"
        // shape.
        let mut remaining: Vec<super::DeferralEntry> =
            Vec::with_capacity(chain.len() + mro_tail.len());
        for i in (0..chain.len() - 1).rev() {
            remaining.push(super::DeferralEntry::Wrapper(chain[i].1.clone()));
        }
        remaining.push(super::DeferralEntry::Candidate {
            owner: owner_class,
            def: Box::new(method_def.clone()),
            wraps_spliced: true,
        });
        remaining.extend(mro_tail);
        self.method_dispatch_stack.push(super::MethodDispatchFrame {
            receiver_class: receiver_class.to_string(),
            invocant,
            args: args.to_vec(),
            remaining,
            rw_params,
            dispatch_token,
            arg_sources,
            // The caller invokes chain's outermost wrapper directly right
            // after this push, so this frame always STARTS inside wrapper
            // code (even when `chain.len() == 1` and `remaining` holds no
            // `Wrapper` entry at all).
            in_wrapper: true,
        });
    }

    pub(crate) fn push_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Value,
    ) -> bool {
        // Always push samewith context so samewith() can find the method name/invocant
        self.push_method_samewith_context(
            receiver_class,
            method_name,
            args,
            Some(invocant.clone()),
        );
        // A user-overridden grammar `parse`/`subparse`/`parsefile` needs an MRO frame
        // even with a single user candidate, so a `nextsame`/`nextwith` inside it can
        // defer to the NATIVE grammar parse — the base candidate that is not a
        // `MethodDef` and so never appears in the dispatch candidates (YAMLish's
        // `method parse` does `nextwith($input, :actions(Actions))`).
        let grammar_parse_override = matches!(method_name, "parse" | "subparse" | "parsefile")
            && self.class_is_grammar(receiver_class)
            && self.has_user_method(receiver_class, method_name);
        // A user BUILDALL/POPULATE/clone (e.g. installed by a custom HOW via
        // `add_method` — OO::Monitors) likewise needs an MRO frame even as a
        // single candidate, so its `callsame` reaches the NATIVE base
        // implementation (`native_mu_base_next_candidate`: the built instance
        // for BUILDALL/POPULATE, the native attribute-copying clone for clone).
        let mu_base_override = matches!(method_name, "BUILDALL" | "POPULATE" | "clone")
            && self.has_user_method(receiver_class, method_name);
        let native_base_override = grammar_parse_override || mu_base_override;
        // Fast path: a name with at most one *structural* dispatch candidate across
        // the MRO can never produce a deferral frame (arg-matching only reduces the
        // candidate count), so skip the per-call `resolve_all_methods_with_owner`
        // MRO walk + MethodDef clones. The structural shape depends only on
        // (class, method), so it is memoized in `dispatch_multi_candidate` and
        // invalidated with the other method caches on any registry change.
        if !native_base_override
            && !self.has_multiple_dispatch_candidates(receiver_class, method_name)
        {
            return false;
        }
        // ADR-0019 E9a: the flat deferral expansion (`resolution_deferral.rs`) replaces
        // `resolve_all_methods_with_owner` as the ordering source — see its module doc for why
        // a raw MRO walk in declaration order does not reproduce raku's own deferral order once
        // a `multi method` spans MRO levels. The expansion is structural (unfiltered); apply the
        // same per-call, invocant-blind argument match `resolve_all_methods_with_owner` used to
        // apply internally.
        let all_candidates = self.matched_deferral_candidates(receiver_class, method_name, args);
        // Fast path: with zero or one candidate there is nothing to defer to, so no
        // dispatch frame is ever pushed (the single candidate is the chosen one and
        // gets skipped, leaving `remaining` empty). Returning early here avoids the
        // per-call `function_body_fingerprint` work below — which Debug-traverses the
        // whole method body AST to derive a candidate identity — for the overwhelmingly
        // common single-method case. Mirrors `push_multi_dispatch_frame`'s `<= 1` guard.
        // A grammar parse / Mu-base override still pushes a frame (empty
        // `remaining`) so its `nextsame`/`nextwith` reaches the native fallback.
        if !native_base_override && all_candidates.len() <= 1 {
            return false;
        }
        // Identify the chosen candidate and skip exactly that one
        let chosen = self.resolve_method_with_owner(receiver_class, method_name, args);
        let chosen_fp = chosen
            .as_ref()
            .map(|(_, def)| self.method_def_fingerprint(def));
        let mut remaining: Vec<(Symbol, super::MethodDef)> = Vec::new();
        let mut skipped_chosen = false;
        for (owner, def) in all_candidates {
            let fp = self.method_def_fingerprint(&def);
            if !skipped_chosen && Some(fp) == chosen_fp {
                skipped_chosen = true;
                continue;
            }
            if self.should_skip_defer_method_candidate(receiver_class, owner.as_str()) {
                continue;
            }
            remaining.push((owner, def));
        }
        // ADR-0019 E8a shadow probe (zero behavior change): see
        // `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`.
        self.shadow_check_deferral_sequence(
            receiver_class,
            method_name,
            args,
            &invocant,
            chosen_fp,
            &remaining,
        );
        let pushed = !remaining.is_empty() || native_base_override;
        if pushed {
            let rw_params = chosen
                .as_ref()
                .map(|(_, def)| {
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs)
                })
                .unwrap_or_default();
            let dispatch_token = self.next_dispatch_token();
            // ADR-0019 E9b-1: every entry is a plain Candidate — this builder
            // never wraps a method's own chain into the frame (that is E9b-2).
            let remaining = remaining
                .into_iter()
                .map(|(owner, def)| super::DeferralEntry::Candidate {
                    owner,
                    def: Box::new(def),
                    wraps_spliced: false,
                })
                .collect();
            self.method_dispatch_stack.push(super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args: args.to_vec(),
                remaining,
                rw_params,
                dispatch_token,
                arg_sources: None,
                in_wrapper: false,
            });
        }
        pushed
    }

    pub(crate) fn has_any_wrap_chains(&self) -> bool {
        self.registry().has_any_method_wrap_chains()
    }

    pub(crate) fn push_wrap_dispatch_frame(&mut self, mut frame: super::WrapDispatchFrame) {
        // ADR-0019 E9b-2: method wraps moved to `MethodDispatchFrame::remaining`
        // (`DeferralEntry::Wrapper`) — this stack now carries SUB wraps only,
        // whose `sub_id` is always a real (non-zero) sub id.
        debug_assert!(
            frame.sub_id != 0,
            "ADR-0019 E9b-2: WrapDispatchFrame is sub-only; sub_id == 0 was the retired method-wrap sentinel"
        );
        frame.dispatch_token = self.next_dispatch_token();
        self.wrap_dispatch_stack.push(frame);
    }

    pub(crate) fn pop_wrap_dispatch_frame(&mut self) {
        self.wrap_dispatch_stack.pop();
    }

    /// Get method-level wrap chain for a specific candidate.
    pub(crate) fn get_method_wrap_chain(
        &self,
        class_name: &str,
        method_name: &str,
        candidate_idx: usize,
    ) -> Option<Vec<(u64, Value)>> {
        self.registry()
            .method_wrap_chain(class_name, method_name, candidate_idx)
            .cloned()
    }

    /// Find the candidate index for a method definition in its class.
    pub(crate) fn find_method_candidate_index(
        &self,
        class_name: &str,
        method_name: &str,
        method_def: &super::MethodDef,
    ) -> Option<usize> {
        // No user-code re-entry here, so a let-bound guard is safe.
        let registry = self.registry();
        let defs = registry.user_method_overloads(class_name, method_name)?;
        // Fast path: a resolved MethodDef is a clone of a registry entry, so
        // its body Arc points at the same allocation — pointer identity finds
        // the candidate without traversing any AST. The structural-fingerprint
        // scan below (which Debug-traverses every candidate's whole body)
        // remains only as a fallback for defs rebuilt with a fresh body Arc
        // (e.g. on-demand recompilation).
        if let Some(idx) = defs
            .iter()
            .position(|d| std::sync::Arc::ptr_eq(&d.body, &method_def.body))
        {
            return Some(idx);
        }
        let target_fp = crate::ast::function_body_fingerprint(
            &method_def.params,
            &method_def.param_defs,
            &method_def.body,
        );
        defs.iter().position(|d| {
            crate::ast::function_body_fingerprint(&d.params, &d.param_defs, &d.body) == target_fp
        })
    }

    /// Pop a method dispatch frame (must only be called if push returned true).
    pub(crate) fn pop_method_dispatch(&mut self) {
        self.method_dispatch_stack.pop();
    }

    /// Pop the samewith context pushed by push_method_dispatch_frame /
    /// push_method_samewith_context.
    /// Must always be called after push_method_dispatch_frame, regardless of its return value.
    pub(crate) fn pop_method_samewith_context(&mut self) {
        if self
            .metamodel_dispatch_stack
            .last()
            .is_some_and(|(depth, ..)| *depth == self.samewith_context_stack.len())
        {
            self.metamodel_dispatch_stack.pop();
        }
        self.pop_samewith_context();
    }

    /// Push a multi dispatch frame for callsame/nextsame/callwith/nextwith support.
    /// Returns true if a frame was pushed (i.e. there are remaining candidates).
    pub(crate) fn push_multi_dispatch_frame(&mut self, name: &str, args: &[Value]) -> bool {
        // Collect ALL multi candidates regardless of arg matching. This is
        // needed because callwith() can re-dispatch with different args, so
        // candidates that don't match the original args may match the new ones.
        let all_candidates = self.resolve_all_multi_candidates(name);
        if all_candidates.len() <= 1 {
            return false;
        }
        // Identify the candidate currently being called by the DETERMINISTIC
        // dispatch winner (the same resolver the interpreter's inline frame uses),
        // NOT a HashMap-ordered first match. `resolve_all_matching_candidates` is
        // HashMap-ordered, so its `.first()` is not reliably the winner: when the
        // narrowest candidate is declared after a broader one, callsame/nextsame
        // would redispatch to the wrong (or the same) candidate, flaking ~50% of
        // the time with the process hash seed. The winner is excluded from
        // `remaining` so redispatch targets the OTHER candidates.
        // Resolve the deterministic winner ONCE (the sound multi-resolution cache
        // skips the registry walk + match/rank for a type+arity-deterministic
        // multi) and reuse it for both the fingerprint identity and the rw-param
        // capture below — the previous code resolved it twice per call.
        let saved_err = self.take_pending_dispatch_error();
        let current_def = self.resolve_function_multi_cached(name, args);
        let current_fp = current_def.as_ref().map(|def| def.body_fingerprint());
        if let Some(err) = saved_err {
            self.set_pending_dispatch_error(err);
        }
        let remaining: Vec<std::sync::Arc<super::FunctionDef>> = all_candidates
            .into_iter()
            .filter(|c| {
                let fp = c.body_fingerprint();
                Some(fp) != current_fp
            })
            .collect();
        let pushed = !remaining.is_empty();
        if pushed {
            // Capture the FIRST (winning) candidate's scalar rw params so a
            // nextsame+rw redispatch can chain the rw value through it (§D).
            let rw_params = current_def
                .as_ref()
                .map(|def| {
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs)
                })
                .unwrap_or_default();
            let dispatch_token = self.next_dispatch_token();
            self.multi_dispatch_stack.push((
                name.to_string(),
                remaining,
                args.to_vec(),
                rw_params,
                dispatch_token,
            ));
        }
        pushed
    }

    /// Pop a multi dispatch frame (must only be called if push returned true).
    pub(crate) fn pop_multi_dispatch(&mut self) {
        self.multi_dispatch_stack.pop();
    }

    /// Push a proto-sub dispatch frame so a compiled proto body's `{*}`
    /// (`__PROTO_DISPATCH__`) can read the original proto args when it
    /// redispatches to the winning multi candidate (ledger §D).
    pub(crate) fn push_proto_dispatch_frame(&mut self, name: String, args: Vec<Value>) {
        self.proto_dispatch_stack.push((name, args, None));
    }

    /// Pop the proto-sub dispatch frame pushed by `push_proto_dispatch_frame`.
    pub(crate) fn pop_proto_dispatch_frame(&mut self) {
        self.proto_dispatch_stack.pop();
    }

    /// Clone of the current proto-dispatch frame `(name, args, method_ctx)`, read
    /// by the VM-native `{*}` redispatch handler. `None` outside a proto body.
    #[allow(clippy::type_complexity)]
    pub(crate) fn proto_dispatch_last(
        &self,
    ) -> Option<(String, Vec<Value>, Option<super::ProtoMethodCtx>)> {
        self.proto_dispatch_stack.last().cloned()
    }

    /// Push a samewith context (ADR-0019 E9c-1: the single push/pop helper
    /// pair every call site funnels through, so `samewith_context_stack`'s
    /// `args` can never desync from `name`/`invocant` the way the former
    /// separate `samewith_call_args_stack` could). `args` is `None` when the
    /// caller has no original-args carrier to attach (a plain sub/proto
    /// samewith context, or a captured `gather`-body re-push) —
    /// `push_method_samewith_context` is the sole caller that passes
    /// `Some(..)`. Always pair with `pop_samewith_context`.
    pub(crate) fn push_samewith_context(
        &mut self,
        name: &str,
        invocant: Option<Value>,
        args: Option<Vec<Value>>,
    ) {
        self.samewith_context_stack.push(super::SamewithContext {
            name: name.to_string(),
            invocant,
            args,
        });
    }

    /// Pop a samewith context.
    pub(crate) fn pop_samewith_context(&mut self) {
        self.samewith_context_stack.pop();
    }

    #[allow(dead_code)]
    pub(crate) fn class_composed_roles(&self, class_name: &str) -> Option<Vec<String>> {
        self.registry()
            .class_composed_roles
            .get(class_name)
            .cloned()
    }

    #[allow(dead_code)]
    pub(crate) fn get_role_def(&self, role_name: &str) -> Option<super::RoleDef> {
        self.registry().roles.get(role_name).cloned()
    }

    pub(crate) fn class_role_param_bindings(
        &self,
        class_name: &str,
    ) -> Option<rustc_hash::FxHashMap<String, Value>> {
        self.registry()
            .class_role_param_bindings
            .get(class_name)
            .cloned()
    }
}

#[cfg(test)]
mod func_multi_cache_generation_tests {
    use super::*;

    // ADR-0019 Phase F box F5: `func_multi_resolve_cache`/`func_multi_type_cacheable`
    // used to depend entirely on the eager clear in `invalidate_method_dispatch_caches`,
    // which only ~7 of the ~20 `fn_resolve_gen`-bumping sites called -- a fresh multi-sub
    // candidate registered at one of the other sites (e.g. `require`, `EVAL`) could leave
    // a stale resolved candidate or a stale cacheable/uncacheable verdict cached under the
    // old name. `refresh_func_multi_caches_for_generation` closes that gap by checking
    // `fn_resolve_gen` at every read, independent of which site bumped it.
    #[test]
    fn stale_entries_are_dropped_when_fn_resolve_gen_advances() {
        let mut i = Interpreter::new();
        let pkg = Symbol::intern("GLOBAL");
        let name = Symbol::intern("f");
        i.func_multi_type_cacheable.insert((pkg, name), true);
        i.func_multi_resolve_cache
            .insert((pkg, name, vec![Symbol::intern("Int")]), None);
        assert!(!i.func_multi_type_cacheable.is_empty());
        assert!(!i.func_multi_resolve_cache.is_empty());

        // No generation change yet: a stale-looking entry is left alone.
        i.refresh_func_multi_caches_for_generation();
        assert!(!i.func_multi_type_cacheable.is_empty());
        assert!(!i.func_multi_resolve_cache.is_empty());

        // Simulate a registration site that bumps `fn_resolve_gen` without going
        // through `invalidate_method_dispatch_caches` (e.g. `require`/`EVAL`).
        i.fn_resolve_gen += 1;
        i.refresh_func_multi_caches_for_generation();
        assert!(i.func_multi_type_cacheable.is_empty());
        assert!(i.func_multi_resolve_cache.is_empty());
        assert_eq!(i.func_multi_cache_generation, i.fn_resolve_gen);
    }

    #[test]
    fn func_multi_dispatch_type_cacheable_self_refreshes() {
        let mut i = Interpreter::new();
        let pkg = Symbol::intern("GLOBAL");
        let name = Symbol::intern("f");
        // Seed a wrong verdict directly (bypassing the real scan) to prove the
        // read path clears it on a generation mismatch rather than trusting it.
        i.func_multi_type_cacheable.insert((pkg, name), true);
        i.fn_resolve_gen += 1;
        // `f` has no registered candidates at all, so a fresh scan answers `false`
        // (not multi). If the stale `true` entry survived, this would wrongly
        // return `true` instead.
        assert!(!i.func_multi_dispatch_type_cacheable(pkg, name, "f"));
    }
}
