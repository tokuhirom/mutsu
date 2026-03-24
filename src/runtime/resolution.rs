use super::*;
use crate::symbol::Symbol;

type CompiledFnMap = std::collections::HashMap<String, crate::opcode::CompiledFunction>;
type ProtectBlockCompiled = std::sync::Arc<crate::opcode::CompiledCode>;
type ProtectBlockCompiledFns = std::sync::Arc<CompiledFnMap>;
type ProtectBlockCapturedBindings = std::sync::Arc<Vec<(usize, String)>>;
type ProtectBlockWritebackBindings = std::sync::Arc<Vec<(usize, String)>>;
type ProtectBlockCapturedNames = std::sync::Arc<Vec<String>>;
/// (pre_phasers, enter_phasers, success_queue, failure_queue, post_phasers, body_main)
pub(super) type SplitPhasers = (
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
);

impl Interpreter {
    const LAZY_GATHER_TAKE_LIMIT_SIGNAL: &str = "__mutsu_lazy_gather_take_limit_reached__";

    fn is_stub_method_body(body: &[Stmt]) -> bool {
        body.len() == 1
            && matches!(
                &body[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

    pub(super) fn resolve_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(&Symbol::intern(name)).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.functions
            .get(&Symbol::intern(&local))
            .cloned()
            .or_else(|| {
                self.functions
                    .get(&Symbol::intern(&format!("GLOBAL::{}", name)))
                    .cloned()
            })
    }

    pub(super) fn insert_token_def(&mut self, name: &str, def: FunctionDef, multi: bool) {
        let key = Symbol::intern(&format!("{}::{}", self.current_package, name));
        if multi {
            self.token_defs.entry(key).or_default().push(def);
        } else {
            self.token_defs.insert(key, vec![def]);
        }
    }

    /// Collect token defs for a given scope (exact + :sym<> variants).
    pub(crate) fn collect_token_defs_for_scope(
        &self,
        scope: &str,
        name: &str,
        defs: &mut Vec<FunctionDef>,
    ) {
        let exact_key = format!("{scope}::{name}");
        if let Some(exact) = self.token_defs.get(&Symbol::intern(&exact_key)) {
            defs.extend(exact.clone());
        }
        let sym_prefix_angle = format!("{scope}::{name}:sym<");
        let sym_prefix_french = format!("{scope}::{name}:sym\u{ab}");
        let mut sym_keys: Vec<String> = self
            .token_defs
            .keys()
            .map(|key| key.resolve())
            .filter(|key| key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french))
            .collect();
        sym_keys.sort();
        for key in &sym_keys {
            if let Some(sym_defs) = self.token_defs.get(&Symbol::intern(key)) {
                defs.extend(sym_defs.clone());
            }
        }
    }

    /// Get parent class names without requiring &mut self (no MRO caching).
    pub(crate) fn class_parents_readonly(&self, class_name: &str) -> Vec<String> {
        if let Some(class_def) = self.classes.get(class_name) {
            if !class_def.mro.is_empty() {
                return class_def.mro.clone();
            }
            return class_def.parents.clone();
        }
        vec![]
    }

    /// Walk MRO (read-only) collecting ancestor names.
    pub(crate) fn mro_readonly(&self, class_name: &str) -> Vec<String> {
        let mut result = vec![class_name.to_string()];
        let mut visited = std::collections::HashSet::new();
        visited.insert(class_name.to_string());
        let mut queue = vec![class_name.to_string()];
        while let Some(current) = queue.pop() {
            for parent in self.class_parents_readonly(&current) {
                if visited.insert(parent.clone()) {
                    result.push(parent.clone());
                    queue.push(parent);
                }
            }
        }
        result
    }

    pub(crate) fn resolve_token_defs(&self, name: &str) -> Option<Vec<FunctionDef>> {
        if name.contains("::") {
            let mut defs = Vec::new();
            if let Some(exact) = self.token_defs.get(&Symbol::intern(name)) {
                defs.extend(exact.clone());
            }
            let sym_prefix_angle = format!("{name}:sym<");
            let sym_prefix_french = format!("{name}:sym\u{ab}");
            let mut sym_keys: Vec<String> = self
                .token_defs
                .keys()
                .map(|key| key.resolve())
                .filter(|key| {
                    key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french)
                })
                .collect();
            sym_keys.sort();
            for key in &sym_keys {
                if let Some(sym_defs) = self.token_defs.get(&Symbol::intern(key)) {
                    defs.extend(sym_defs.clone());
                }
            }
            // If not found, try walking MRO of the package part
            if defs.is_empty()
                && let Some(pos) = name.rfind("::")
            {
                let pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                for ancestor in self.mro_readonly(pkg) {
                    if ancestor == pkg {
                        continue; // already checked
                    }
                    self.collect_token_defs_for_scope(&ancestor, token_name, &mut defs);
                    if !defs.is_empty() {
                        break;
                    }
                }
            }
            return if defs.is_empty() { None } else { Some(defs) };
        }
        let mut defs = Vec::new();
        // Check current package and its MRO
        let scopes_to_check = self.mro_readonly(&self.current_package);
        for scope in &scopes_to_check {
            self.collect_token_defs_for_scope(scope, name, &mut defs);
            if !defs.is_empty() {
                break;
            }
        }
        // Also check GLOBAL
        if defs.is_empty() {
            self.collect_token_defs_for_scope("GLOBAL", name, &mut defs);
        }
        if defs.is_empty() { None } else { Some(defs) }
    }

    pub(crate) fn has_proto_token(&self, name: &str) -> bool {
        if name.contains("::") {
            if self.proto_tokens.contains(name) {
                return true;
            }
            // Walk MRO for qualified names
            if let Some(pos) = name.rfind("::") {
                let pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                for ancestor in self.mro_readonly(pkg) {
                    if ancestor == pkg {
                        continue;
                    }
                    if self
                        .proto_tokens
                        .contains(&format!("{ancestor}::{token_name}"))
                    {
                        return true;
                    }
                }
            }
            return false;
        }
        // Check current package MRO
        for scope in self.mro_readonly(&self.current_package) {
            if self.proto_tokens.contains(&format!("{scope}::{name}")) {
                return true;
            }
        }
        self.proto_tokens.contains(&format!("GLOBAL::{}", name))
    }

    pub(super) fn resolve_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<MethodDef> {
        self.resolve_method_with_owner(class_name, method_name, arg_values)
            .map(|(_, def)| def)
    }

    pub(crate) fn resolve_method_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        // Collect all matching multi candidates across the MRO, then pick the
        // most specific one by type hierarchy distance.
        let mut all_matches: Vec<(String, MethodDef)> = Vec::new();
        // Track whether a non-multi submethod was found on an ancestor
        // (submethods block MRO search for their class but not for
        // descendants).
        let mut submethod_blocks = false;
        for cn in &mro {
            if let Some(overloads) = self
                .classes
                .get(cn.as_str())
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                let any_multi = overloads.iter().any(|d| d.is_multi);
                // Check if all overloads are submethods on an ancestor class
                let all_submethods = overloads.iter().all(|d| d.is_my);
                let is_ancestor = cn != class_name;
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited: skip if defined on an
                    // ancestor class rather than the receiver's own class.
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        if !any_multi {
                            // Non-multi: return the first match immediately
                            return Some((cn.clone(), def));
                        }
                        all_matches.push((cn.clone(), def));
                    }
                }
                // A non-multi submethod on an ancestor blocks the MRO search
                // (the method name exists but no candidate matched and
                // submethods are not inherited).
                if !any_multi && is_ancestor && all_submethods {
                    submethod_blocks = true;
                    continue;
                }
                // Method name is present on this class, but no candidate matched.
                // For non-multi methods, stop here — a subclass override
                // hides the parent's version.
                if !any_multi && all_matches.is_empty() {
                    return None;
                }
            }
        }
        if all_matches.len() <= 1 {
            return all_matches.into_iter().next();
        }
        // Pick the candidate with the smallest type hierarchy distance
        let mut best_idx = 0;
        let mut best_dist = self.method_candidate_type_distance(arg_values, &all_matches[0].1);
        for (i, (_, def)) in all_matches.iter().enumerate().skip(1) {
            let dist = self.method_candidate_type_distance(arg_values, def);
            if dist < best_dist {
                best_dist = dist;
                best_idx = i;
            }
        }
        let _ = submethod_blocks; // used for control flow above
        Some(all_matches.remove(best_idx))
    }

    /// Compute the type distance of a method's param constraints from the
    /// actual arguments.  Lower distance = more specific match.
    fn method_candidate_type_distance(&self, args: &[Value], def: &MethodDef) -> usize {
        let mut total = 0usize;
        let mut arg_idx = 0;
        for pd in &def.param_defs {
            if pd.is_invocant || pd.named {
                continue;
            }
            if let Some(tc) = &pd.type_constraint {
                let base = Self::constraint_base_for_distance(tc);
                if arg_idx < args.len() {
                    total += Self::builtin_type_distance(base, &args[arg_idx]);
                }
            } else {
                total += 1000;
            }
            arg_idx += 1;
        }
        total
    }

    fn constraint_base_for_distance(constraint: &str) -> &str {
        let s = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        s.split('(').next().unwrap_or(s)
    }

    /// Compute the type hierarchy distance between a constraint and a value.
    /// 0 = exact match, larger = less specific.
    fn builtin_type_distance(constraint: &str, value: &Value) -> usize {
        let value_type = super::value_type_name(value);
        if constraint == value_type {
            return 0;
        }
        if let Value::Instance { class_name, .. } = value
            && constraint == class_name.resolve().as_str()
        {
            return 0;
        }
        let builtin_mro: &[&str] = match value_type {
            "Bool" => &["Bool", "Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Int" => &["Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Rat" | "FatRat" => &["Rat", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Complex" => &["Complex", "Numeric", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Stringy", "Cool", "Any", "Mu"],
            "Array" => &["Array", "List", "Positional", "Cool", "Any", "Mu"],
            "List" => &["List", "Positional", "Cool", "Any", "Mu"],
            "Hash" => &["Hash", "Map", "Associative", "Cool", "Any", "Mu"],
            "Pair" => &["Pair", "Associative", "Cool", "Any", "Mu"],
            "Range" => &["Range", "Positional", "Cool", "Any", "Mu"],
            "Set" => &["Set", "Setty", "QuantHash", "Associative", "Any", "Mu"],
            "Bag" => &["Bag", "Baggy", "QuantHash", "Associative", "Any", "Mu"],
            "Mix" => &[
                "Mix",
                "Mixy",
                "Baggy",
                "QuantHash",
                "Associative",
                "Any",
                "Mu",
            ],
            "Sub" => &["Sub", "Routine", "Block", "Code", "Callable", "Any", "Mu"],
            "Seq" => &["Seq", "Positional", "Cool", "Any", "Mu"],
            "Regex" => &["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"],
            "Junction" => &["Junction", "Mu"],
            _ => &[],
        };
        for (i, &ancestor) in builtin_mro.iter().enumerate() {
            if ancestor == constraint {
                return i;
            }
        }
        500
    }

    pub(crate) fn resolve_all_methods_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mut matches = Vec::new();
        for cn in &mro {
            if let Some(overloads) = self
                .classes
                .get(cn.as_str())
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                let is_ancestor = cn != class_name;
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        matches.push((cn.clone(), def));
                    }
                }
            }
        }
        matches
    }

    pub(crate) fn should_skip_defer_method_candidate(
        &self,
        receiver_class: &str,
        candidate_owner: &str,
    ) -> bool {
        if receiver_class != candidate_owner && self.hidden_classes.contains(candidate_owner) {
            return true;
        }
        self.hidden_defer_parents
            .get(receiver_class)
            .is_some_and(|hidden| hidden.contains(candidate_owner))
    }

    pub(super) fn resolve_private_method_with_owner(
        &mut self,
        class_name: &str,
        owner_class: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if cn != owner_class {
                continue;
            }
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def));
                    }
                }
            }
        }
        None
    }

    pub(super) fn resolve_private_method_any_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        if arg_values.is_empty()
            && let Some(cached) = self
                .private_zeroarg_method_cache
                .get(&(class_name.to_string(), method_name.to_string()))
        {
            return cached.clone();
        }
        let mro = self.class_mro(class_name);
        // Fast path: when there are no positional args, avoid cloning the
        // overloads vector by scanning with a shared borrow first. This covers
        // the common case of zero-argument private method calls in tight loops.
        if arg_values.is_empty() {
            for cn in &mro {
                if let Some(overloads) = self
                    .classes
                    .get(cn)
                    .and_then(|c| c.methods.get(method_name))
                {
                    // First pass: skip stubs
                    for def in overloads {
                        if !def.is_private {
                            continue;
                        }
                        if Self::is_stub_method_body(&def.body) {
                            continue;
                        }
                        if def
                            .param_defs
                            .iter()
                            .all(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                        {
                            let resolved = Some((cn.clone(), def.clone()));
                            self.private_zeroarg_method_cache.insert(
                                (class_name.to_string(), method_name.to_string()),
                                resolved.clone(),
                            );
                            return resolved;
                        }
                    }
                    // Second pass: include stubs
                    for def in overloads {
                        if !def.is_private {
                            continue;
                        }
                        if def
                            .param_defs
                            .iter()
                            .all(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                        {
                            let resolved = Some((cn.clone(), def.clone()));
                            self.private_zeroarg_method_cache.insert(
                                (class_name.to_string(), method_name.to_string()),
                                resolved.clone(),
                            );
                            return resolved;
                        }
                    }
                }
            }
        }
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in &overloads {
                    if !def.is_private {
                        continue;
                    }
                    if Self::is_stub_method_body(&def.body) {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def.clone()));
                    }
                }
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def));
                    }
                }
            }
        }
        None
    }

    pub(crate) fn resolve_private_method_for_vm(
        &mut self,
        class_name: &str,
        method: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let private_rest = method.strip_prefix('!')?;
        if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
            self.resolve_private_method_with_owner(class_name, owner_class, pm_name, arg_values)
        } else {
            self.resolve_private_method_any_owner(class_name, private_rest, arg_values)
        }
    }

    pub(crate) fn can_fast_dispatch_private_method_vm(&self, owner_class: &str) -> bool {
        self.method_class_stack
            .last()
            .is_some_and(|caller| caller == owner_class)
    }

    pub(super) fn class_mro(&mut self, class_name: &str) -> Vec<String> {
        if !self.classes.contains_key(class_name)
            && let Some((base, _)) = class_name.split_once('[')
            && class_name.ends_with(']')
            && self.classes.contains_key(base)
        {
            let mut mro = vec![class_name.to_string()];
            mro.extend(self.class_mro(base));
            return mro;
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.clone();
        }
        let mut stack = Vec::new();
        match self.compute_class_mro(class_name, &mut stack) {
            Ok(mro) => {
                if let Some(class_def) = self.classes.get_mut(class_name) {
                    class_def.mro = mro.clone();
                }
                mro
            }
            Err(_) => vec![class_name.to_string()],
        }
    }

    pub(super) fn force_lazy_list(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }
        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        self.gather_take_limits.push(None);
        let run_res = self.run_block(&list.body);
        let items = self.gather_items.pop().unwrap_or_default();
        self.gather_take_limits.pop();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
            self.gather_take_limits.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert(k.clone(), v.clone());
        }
        self.env = merged_env;
        run_res?;
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    pub(crate) fn force_lazy_list_prefix_bridge(
        &mut self,
        list: &crate::value::LazyList,
        needed_len: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        if needed_len == 0 {
            return Ok(Vec::new());
        }
        if let Some(cached) = list.cache.lock().unwrap().clone()
            && cached.len() >= needed_len
        {
            return Ok(cached[..needed_len].to_vec());
        }

        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        self.gather_take_limits.push(Some(needed_len));
        let run_res = self.run_block(&list.body);
        let mut items = self.gather_items.pop().unwrap_or_default();
        self.gather_take_limits.pop();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
            self.gather_take_limits.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert(k.clone(), v.clone());
        }
        self.env = merged_env;

        match run_res {
            Ok(()) => {
                *list.cache.lock().unwrap() = Some(items.clone());
            }
            Err(err) if err.message == Self::LAZY_GATHER_TAKE_LIMIT_SIGNAL => {}
            Err(err) => return Err(err),
        }
        if items.len() > needed_len {
            items.truncate(needed_len);
        }
        Ok(items)
    }

    pub(crate) fn force_lazy_list_bridge(
        &mut self,
        list: &crate::value::LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.force_lazy_list(list)
    }

    pub(super) fn split_block_phasers(&self, stmts: &[Stmt]) -> SplitPhasers {
        let mut pre_ph = Vec::new();
        let mut enter_ph = Vec::new();
        let mut body_main = Vec::new();
        let mut success_queue = Vec::new();
        let mut failure_queue = Vec::new();
        let mut post_ph = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Pre => pre_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Post | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo => {}
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        // POST phasers in reverse source order
        for stmt in stmts.iter().rev() {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Leave => {
                        success_queue.push(Stmt::Block(body.clone()));
                        failure_queue.push(Stmt::Block(body.clone()));
                    }
                    PhaserKind::Keep => success_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Undo => failure_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Post => post_ph.push(Stmt::Block(body.clone())),
                    _ => {}
                }
            }
        }
        (
            pre_ph,
            enter_ph,
            success_queue,
            failure_queue,
            post_ph,
            body_main,
        )
    }

    pub(super) fn make_promise_instance(&self, status: &str, result: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str(status.to_string()));
        attrs.insert("result".to_string(), result);
        Value::make_instance(Symbol::intern("Promise"), attrs)
    }

    pub(super) fn make_supply_instance(&self) -> Value {
        let sid = super::native_methods::next_supply_id();
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::Int(sid as i64));
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    pub(crate) fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let func = match func {
            Value::WeakSub(ref weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            },
            other => other,
        };
        if let Value::Routine { package, name, .. } = &func {
            // Try fully-qualified name first (e.g. "A::foo"), then bare name
            if package != "" && package != "GLOBAL" {
                let fq = format!("{package}::{name}");
                if self.resolve_function(&fq).is_some() {
                    return self.call_function(&fq, args);
                }
            }
            return self.call_function(&name.resolve(), args);
        }
        if let Value::Junction { kind, values } = func {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.call_sub_value(callable.clone(), args.clone(), merge_all)?);
            }
            return Ok(Value::junction(kind, results));
        }
        if let Value::Sub(data) = func {
            // Check for wrap chain — if wrappers exist, dispatch through them
            // Skip if we're already inside a wrap dispatch for this sub
            let already_dispatching = self.wrap_dispatch_stack.iter().any(|f| f.sub_id == data.id);
            if !already_dispatching
                && let Some(chain) = self.wrap_chains.get(&data.id).cloned()
                && !chain.is_empty()
            {
                let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
                self.test_pending_callsite_line = callsite_line;
                // Build remaining list: inner wrappers then original sub
                // chain is ordered inner-to-outer (last = outermost), so:
                // outermost is last, we call it; remaining = [inner..., original]
                let outermost = chain.last().unwrap().1.clone();
                let mut remaining: Vec<Value> = Vec::new();
                // Add wrappers from second-to-last down to first (inner order)
                for i in (0..chain.len() - 1).rev() {
                    remaining.push(chain[i].1.clone());
                }
                // Add the original sub at the end
                remaining.push(Value::Sub(data.clone()));
                let frame = super::WrapDispatchFrame {
                    sub_id: data.id,
                    remaining,
                    args: sanitized_args.clone(),
                };
                self.wrap_dispatch_stack.push(frame);
                let result = self.call_sub_value(outermost, sanitized_args, false);
                self.wrap_dispatch_stack.pop();
                return result;
            }
            let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
            self.test_pending_callsite_line = callsite_line;
            let mut call_args = sanitized_args.clone();
            if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
                let mut positional = Vec::new();
                let mut named = data.assumed_named.clone();
                let mut incoming_positional = Vec::new();
                for arg in &sanitized_args {
                    if let Value::Pair(key, boxed) = arg {
                        named.insert(key.clone(), *boxed.clone());
                    } else {
                        incoming_positional.push(arg.clone());
                    }
                }
                // Fill bare `*` primers from incoming positional args in order.
                // Remaining positional args are appended after all fixed primers.
                let mut incoming_idx = 0usize;
                for assumed in &data.assumed_positional {
                    let is_placeholder = matches!(assumed, Value::Whatever)
                        || matches!(assumed, Value::Num(f) if f.is_infinite())
                        || matches!(assumed, Value::Rat(_, 0));
                    if is_placeholder {
                        if incoming_idx < incoming_positional.len() {
                            positional.push(incoming_positional[incoming_idx].clone());
                            incoming_idx += 1;
                        }
                    } else {
                        positional.push(assumed.clone());
                    }
                }
                positional.extend(incoming_positional.into_iter().skip(incoming_idx));
                call_args = positional;
                for (key, value) in named {
                    call_args.push(Value::Pair(key, Box::new(value)));
                }
            }
            // Routine wrapper from .assuming() on a multi-dispatch sub
            if let Some(Value::Str(routine_name)) = data.env.get("__mutsu_routine_name").cloned() {
                return self.call_function(&routine_name, call_args);
            }
            if let (Some(left), Some(right)) = (
                data.env.get("__mutsu_compose_left").cloned(),
                data.env.get("__mutsu_compose_right").cloned(),
            ) {
                let right_result = self.call_sub_value(right, call_args, false)?;
                let (left_params, left_param_defs) = self.callable_signature(&left);
                let left_variadic = left_param_defs.iter().any(|pd| pd.slurpy && !pd.named);
                let left_expects_single = match &left {
                    Value::Sub(left_data) if left_params.is_empty() => {
                        let (uses_positional, _) = Self::auto_signature_uses(&left_data.body);
                        !uses_positional
                    }
                    _ => !left_variadic && left_params.len() == 1,
                };
                let left_args = Self::composed_result_to_args(right_result, left_expects_single);
                return self.call_sub_value(left, left_args, false);
            }
            let saved_env = self.env.clone();
            let saved_readonly = self.save_readonly_vars();
            if let Some(line) = self.test_pending_callsite_line {
                self.env.insert("?LINE".to_string(), Value::Int(line));
            }
            self.push_caller_env();
            let persist_closure_env = data.name == "" || !self.has_function(&data.name.resolve());
            let closure_base_env = if persist_closure_env {
                self.closure_env_overrides
                    .get(&data.id)
                    .cloned()
                    .unwrap_or_else(|| data.env.clone())
            } else {
                data.env.clone()
            };
            let mut new_env = saved_env.clone();
            for (k, v) in &closure_base_env {
                if merge_all {
                    new_env.entry(k.clone()).or_insert(v.clone());
                    continue;
                }
                if matches!(new_env.get(k), Some(Value::Array(..))) && matches!(v, Value::Array(..))
                {
                    continue;
                }
                new_env.insert(k.clone(), v.clone());
            }
            self.env = new_env.clone();
            let rw_bindings =
                match self.bind_function_args_values(&data.param_defs, &data.params, &call_args) {
                    Ok(bindings) => bindings,
                    Err(e) => {
                        self.pop_caller_env();
                        self.env = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        return Err(e);
                    }
                };
            new_env = self.env.clone();
            if data.params.is_empty() {
                for arg in &sanitized_args {
                    if let Value::Pair(name, value) = arg {
                        new_env.insert(format!(":{}", name), *value.clone());
                    }
                }
            }
            // Bind implicit $_ for bare blocks called with arguments
            let (uses_positional, _) = Self::auto_signature_uses(&data.body);
            if data.params.is_empty()
                && !uses_positional
                && !sanitized_args.is_empty()
                && let Some(first_positional) = sanitized_args
                    .iter()
                    .find(|v| !matches!(v, Value::Pair(_, _)))
            {
                new_env.insert("_".to_string(), first_positional.clone());
                new_env.insert("$_".to_string(), first_positional.clone());
            } else if data.params.is_empty()
                && sanitized_args.is_empty()
                && data.name == ""
                && let Some(caller_topic) = saved_env.get("_")
            {
                new_env.insert("_".to_string(), caller_topic.clone());
                new_env.insert("$_".to_string(), caller_topic.clone());
            }
            // &?BLOCK: weak self-reference to break reference cycles
            let block_arc = std::sync::Arc::new(crate::value::SubData {
                package: data.package,
                name: data.name,
                params: data.params.clone(),
                param_defs: data.param_defs.clone(),
                body: data.body.clone(),
                is_rw: data.is_rw,
                is_raw: data.is_raw,
                env: new_env.clone(),
                assumed_positional: data.assumed_positional.clone(),
                assumed_named: data.assumed_named.clone(),
                id: data.id,
                empty_sig: data.empty_sig,
                is_bare_block: data.is_bare_block,
                compiled_code: data.compiled_code.clone(),
            });
            new_env.insert(
                "&?BLOCK".to_string(),
                Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
            );
            let block_sub = Value::make_sub_with_id(
                data.package,
                data.name,
                vec![],
                Vec::new(),
                data.body.clone(),
                data.is_rw,
                new_env.clone(),
                data.id,
            );
            self.env = new_env;
            self.env.insert(
                "__mutsu_callable_id".to_string(),
                Value::Int(data.id as i64),
            );
            self.routine_stack
                .push((data.package.resolve(), data.name.resolve()));
            self.block_stack.push(block_sub);
            let return_spec = data.env.get("__mutsu_return_type").and_then(|v| match v {
                Value::Str(s) => Some(s.to_string()),
                _ => None,
            });
            self.prepare_definite_return_slot(return_spec.as_deref());
            let let_mark = self.let_saves.len();
            let result = match self.eval_block_value(&data.body) {
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", data.package, data.name);
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        target_id == data.id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        if e.return_value.is_none() {
                            e.return_value = Some(Value::Nil);
                        }
                        Err(e)
                    } else {
                        Err(e)
                    }
                }
                other => other,
            };
            self.block_stack.pop();
            self.routine_stack.pop();
            // Manage let saves based on sub result
            match &result {
                Ok(_) => {
                    // Explicit return or successful completion — discard saves
                    self.discard_let_saves(let_mark);
                }
                Err(e) if e.return_value.is_some() => {
                    // Explicit return — discard saves
                    self.discard_let_saves(let_mark);
                }
                Err(_) => {
                    // Exception/fail — restore saves
                    self.restore_let_saves(let_mark);
                }
            }
            if persist_closure_env {
                let mut persisted_closure_env = closure_base_env.clone();
                for key in closure_base_env.keys() {
                    if let Some(value) = self.env.get(key).cloned() {
                        persisted_closure_env.insert(key.clone(), value);
                    }
                }
                self.closure_env_overrides
                    .insert(data.id, persisted_closure_env);
            }
            // Preserve map rw topic writeback across env restoration
            let rw_map_topic = self.env.get("__mutsu_rw_map_topic__").cloned();
            let mut merged = saved_env;
            self.pop_caller_env_with_writeback(&mut merged);
            if merge_all {
                for (k, v) in self.env.iter() {
                    if k != "_"
                        && k != "@_"
                        && (merged.contains_key(k) || k.starts_with("__mutsu_var_meta::"))
                    {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            } else {
                for (k, v) in self.env.iter() {
                    if k != "_" && k != "@_" && matches!(v, Value::Array(..)) {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            }
            self.merge_sigilless_alias_writes(&mut merged, &self.env);
            // Apply rw bindings after merge so they take precedence
            self.apply_rw_bindings_to_env(&rw_bindings, &mut merged);
            // Restore map rw topic tracker if it was set during block execution
            if let Some(topic_val) = rw_map_topic {
                merged.insert("__mutsu_rw_map_topic__".to_string(), topic_val);
            }
            self.env = merged;
            self.restore_readonly_vars(saved_readonly);
            if let Err(e) = &result
                && e.is_fail
            {
                return Ok(self.fail_error_to_failure_value(e));
            }
            let result = match result {
                Err(e) if e.is_leave => return Err(e),
                other => other,
            };
            let finalized = self.finalize_return_with_spec(result, return_spec.as_deref());
            let fetch_rw = data.is_rw && !data.is_raw;
            return finalized.and_then(|v| {
                let v = if let Value::LazyList(list) = v {
                    let mut env = list.env.clone();
                    env.insert(
                        "__mutsu_preserve_lazy_on_array_assign".to_string(),
                        Value::Bool(true),
                    );
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList {
                        body: list.body.clone(),
                        env,
                        cache: std::sync::Mutex::new(list.cache.lock().unwrap().clone()),
                        compiled_code: list.compiled_code.clone(),
                        compiled_fns: list.compiled_fns.clone(),
                    }))
                } else {
                    v
                };
                self.maybe_fetch_rw_proxy(v, fetch_rw)
            });
        }
        Err(RuntimeError::new("Callable expected"))
    }

    fn composed_result_to_args(value: Value, prefer_single: bool) -> Vec<Value> {
        if prefer_single {
            return match value {
                Value::Seq(items) | Value::Slip(items) => vec![Value::array(items.to_vec())],
                other => vec![other],
            };
        }
        match value {
            Value::Array(items, _) => items.to_vec(),
            Value::Seq(items) => items.to_vec(),
            Value::Slip(items) => items.to_vec(),
            Value::Capture { positional, named } => {
                let mut args = positional;
                for (k, v) in named {
                    args.push(Value::Pair(k, Box::new(v)));
                }
                args
            }
            other => vec![other],
        }
    }

    /// Like `eval_block_value` but handles PRE/POST phasers in the body.
    /// Used for function call evaluation where the body may contain PRE/POST.
    pub(crate) fn eval_block_value_with_pre_post(
        &mut self,
        body: &[Stmt],
    ) -> Result<Value, RuntimeError> {
        let (pre_ph, _enter_ph, _success_ph, _failure_ph, post_ph, body_main) =
            self.split_block_phasers(body);
        if pre_ph.is_empty() && post_ph.is_empty() {
            return self.eval_block_value(body);
        }
        // Run PRE phasers
        for pre in &pre_ph {
            let result = self.eval_block_value(std::slice::from_ref(pre))?;
            if !result.truthy() {
                return Err(Self::make_phaser_prepost_error(true));
            }
        }
        // Run main body
        let result = self.eval_block_value(&body_main);
        // Run POST phasers (with $_ set to return value for POST checks)
        let ret_val = match &result {
            Ok(v) => v.clone(),
            Err(e) => e.return_value.clone().unwrap_or(Value::Nil),
        };
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), ret_val);
        for post in &post_ph {
            let post_result = self.eval_block_value(std::slice::from_ref(post));
            match post_result {
                Ok(v) if !v.truthy() => {
                    if let Some(t) = saved_topic {
                        self.env.insert("_".to_string(), t);
                    }
                    return Err(Self::make_phaser_prepost_error(false));
                }
                Err(e) => {
                    if let Some(t) = saved_topic {
                        self.env.insert("_".to_string(), t);
                    }
                    return Err(e);
                }
                _ => {}
            }
        }
        if let Some(t) = saved_topic {
            self.env.insert("_".to_string(), t);
        }
        result
    }

    pub(crate) fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        let let_mark = self.let_saves_len();
        let saved_functions = self.functions.clone();
        let saved_proto_subs = self.proto_subs.clone();
        let saved_proto_functions = self.proto_functions.clone();
        let saved_operator_assoc = self.operator_assoc.clone();
        let saved_code_env: std::collections::HashMap<String, Value> = self
            .env
            .iter()
            .filter(|(k, _)| k.starts_with('&') || k.starts_with("__mutsu_callable_id::"))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let mut compiler = crate::compiler::Compiler::new();
        compiler.is_routine = !self.routine_stack.is_empty();
        let scope = if let Some((pkg, routine)) = self.routine_stack.last() {
            format!("{}::&{}", pkg, routine)
        } else {
            self.current_package.clone()
        };
        compiler.set_current_package(scope);
        let (code, compiled_fns) = compiler.compile(body);
        self.block_scope_depth += 1;
        let result = self.run_compiled_block(&code, &compiled_fns);
        let trailing_sub_value = match body.last() {
            Some(Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                is_rw,
                ..
            }) => Some(Value::make_sub(
                Symbol::intern(&self.current_package),
                *name,
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                self.env.clone(),
            )),
            _ => None,
        };
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
        // Sub/proto declarations in block scope are lexical; restore registries on block exit.
        self.functions = saved_functions;
        self.proto_subs = saved_proto_subs;
        self.proto_functions = saved_proto_functions;
        self.operator_assoc = saved_operator_assoc;
        self.env
            .retain(|k, _| !(k.starts_with('&') || k.starts_with("__mutsu_callable_id::")));
        for (k, v) in saved_code_env {
            self.env.insert(k, v);
        }
        // Blocks are scope boundaries for temp/let saves.
        self.restore_let_saves(let_mark);
        self.run_pending_instance_destroys()?;
        result.map(|value| {
            let missing_value = matches!(value, Value::Nil)
                || matches!(&value, Value::Package(name) if name == "Any");
            if missing_value {
                trailing_sub_value.unwrap_or(Value::Nil)
            } else {
                value
            }
        })
    }

    /// Fast path for simple closures (e.g. sequence generators) that don't
    /// declare subs or modify proto registries. Takes pre-compiled bytecode
    /// to avoid recompilation on every call.
    pub(crate) fn eval_precompiled_block_fast(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.block_scope_depth += 1;
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (mut interp, result) = vm.run(code, compiled_fns);
        for (slot, key) in &code.state_locals {
            if let Some(name) = code.locals.get(*slot)
                && let Some(val) = interp.env().get(name).cloned()
            {
                interp.set_state_var(key.clone(), val);
            }
        }
        let value = interp.env().get("_").cloned().unwrap_or(Value::Nil);
        *self = interp;
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
        result.map(|last_value| last_value.unwrap_or(value))
    }

    /// Fast path for `Lock::Async.protect { ... }` — executes a bare block
    /// directly in the current env without the full env save/restore overhead
    /// of `call_sub_value`.  Shared vars must already be synced to env by the
    /// caller.
    pub(crate) fn call_protect_block(&mut self, code: &Value) -> Result<Value, RuntimeError> {
        if let Value::Sub(data) = code {
            let (compiled, compiled_fns, _captured_bindings, _writeback_bindings, captured_names) =
                self.get_or_compile_protect_block_with_slots(data);
            self.sync_shared_vars_for_names(captured_names.iter().map(|name| name.as_str()));
            self.run_compiled_block(&compiled, compiled_fns.as_ref())
        } else {
            self.call_sub_value(code.clone(), Vec::new(), true)
        }
    }

    pub(crate) fn get_or_compile_protect_block_with_slots(
        &mut self,
        data: &std::sync::Arc<crate::value::SubData>,
    ) -> (
        ProtectBlockCompiled,
        ProtectBlockCompiledFns,
        ProtectBlockCapturedBindings,
        ProtectBlockWritebackBindings,
        ProtectBlockCapturedNames,
    ) {
        let entry = self.protect_block_cache.entry(data.id).or_insert_with(|| {
            let (compiled, compiled_fns) = if let Some(ref cc) = data.compiled_code {
                (
                    cc.clone(),
                    std::sync::Arc::new(std::collections::HashMap::new()),
                )
            } else {
                let compiler = crate::compiler::Compiler::new();
                let (compiled, compiled_fns) = compiler.compile(&data.body);
                (
                    std::sync::Arc::new(compiled),
                    std::sync::Arc::new(compiled_fns),
                )
            };
            let captured_bindings: Vec<(usize, String)> = compiled
                .locals
                .iter()
                .enumerate()
                .filter(|(_, name)| data.env.contains_key(*name))
                .map(|(idx, name)| (idx, name.clone()))
                .collect();
            let mut assigned_slots = std::collections::HashSet::new();
            for op in &compiled.ops {
                match op {
                    crate::opcode::OpCode::SetLocal(slot)
                    | crate::opcode::OpCode::AssignExprLocal(slot)
                    | crate::opcode::OpCode::PreIncrement(slot)
                    | crate::opcode::OpCode::PreDecrement(slot)
                    | crate::opcode::OpCode::PostIncrement(slot)
                    | crate::opcode::OpCode::PostDecrement(slot) => {
                        assigned_slots.insert(*slot as usize);
                    }
                    _ => {}
                }
            }
            let writeback_bindings: Vec<(usize, String)> = captured_bindings
                .iter()
                .filter(|(slot, _)| assigned_slots.contains(slot))
                .cloned()
                .collect();
            let mut captured_names: Vec<String> = captured_bindings
                .iter()
                .map(|(_, name)| name.clone())
                .collect();
            for op in &compiled.ops {
                let name_idx = match op {
                    crate::opcode::OpCode::GetGlobal(idx)
                    | crate::opcode::OpCode::SetGlobal(idx)
                    | crate::opcode::OpCode::GetArrayVar(idx)
                    | crate::opcode::OpCode::GetHashVar(idx)
                    | crate::opcode::OpCode::CheckReadOnly(idx) => Some(*idx as usize),
                    _ => None,
                };
                let Some(idx) = name_idx else {
                    continue;
                };
                let Some(crate::value::Value::Str(name)) = compiled.constants.get(idx) else {
                    continue;
                };
                if data.env.contains_key(name.as_str()) && !captured_names.contains(name) {
                    captured_names.push(name.to_string());
                }
            }
            (
                compiled,
                compiled_fns,
                std::sync::Arc::new(captured_bindings),
                std::sync::Arc::new(writeback_bindings),
                std::sync::Arc::new(captured_names),
            )
        });
        (
            entry.0.clone(),
            entry.1.clone(),
            entry.2.clone(),
            entry.3.clone(),
            entry.4.clone(),
        )
    }

    /// Run pre-compiled bytecode and return the `$_` topic value.
    fn run_compiled_block(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (mut interp, result) = vm.run(code, compiled_fns);
        // Persist state variables for top-level compiled blocks too.
        for (slot, key) in &code.state_locals {
            if let Some(name) = code.locals.get(*slot)
                && let Some(val) = interp.env().get(name).cloned()
            {
                interp.set_state_var(key.clone(), val);
            }
        }
        let value = interp.env().get("_").cloned().unwrap_or(Value::Nil);
        *self = interp;
        result.map(|last_value| last_value.unwrap_or(value))
    }

    pub(super) fn eval_map_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub(data)) = func {
            let requires_full_binding = data.param_defs.iter().any(|pd| {
                pd.named
                    || pd.slurpy
                    || pd.sigilless
                    || pd.optional_marker
                    || pd.default.is_some()
                    || pd.type_constraint.is_some()
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
                    || pd.shape_constraints.is_some()
            });
            if requires_full_binding {
                let mut result = Vec::new();
                for item in list_items {
                    let value = self.call_sub_value(Value::Sub(data.clone()), vec![item], false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                }
                return Ok(Value::array(result));
            }
            let arity = if !data.params.is_empty() {
                // Account for assumed positional args (from .assuming)
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            // Routine wrapper from .assuming() on a builtin — delegate to call_sub_value
            // which knows how to resolve __mutsu_routine_name.
            if data.env.contains_key("__mutsu_routine_name") {
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            // Save/restore only temporary bindings introduced by map itself.
            // Captured lexical vars (in data.env) must keep mutations done inside
            // the mapper block (e.g. `{ $a++ }`).
            let mut touched_keys: Vec<String> = Vec::with_capacity(data.params.len() + 1);
            for k in data.env.keys() {
                if !self.env.contains_key(k) {
                    touched_keys.push(k.clone());
                }
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            if !touched_keys.iter().any(|k| k == "$_") {
                touched_keys.push(dollar_topic.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                if !self.env.contains_key(k) {
                    self.env.insert(k.clone(), v.clone());
                }
            }

            // Create VM once, reuse across iterations
            let interp = std::mem::take(self);
            let mut vm = crate::vm::VM::new(interp);

            let mut i = 0usize;
            while i < list_items.len() {
                if arity > 1 && i + arity > list_items.len() {
                    *self = vm.into_interpreter();
                    for (k, orig) in &saved {
                        match orig {
                            Some(v) => {
                                self.env.insert(k.clone(), v.clone());
                            }
                            None => {
                                self.env.remove(k);
                            }
                        }
                    }
                    return Err(RuntimeError::new("Not enough elements for map block arity"));
                }
                {
                    let interp = vm.interpreter_mut();
                    let assumed_count = data.assumed_positional.len();
                    // Bind assumed positional args first
                    for (idx, val) in data.assumed_positional.iter().enumerate() {
                        if let Some(p) = data.params.get(idx) {
                            interp.env_insert(p.clone(), val.clone());
                        }
                    }
                    if arity == 1 {
                        let item = list_items[i].clone();
                        if let Some(p) = data.params.get(assumed_count) {
                            interp.env_insert(p.clone(), item.clone());
                        }
                        interp.env_insert(underscore.clone(), item.clone());
                        interp.env_insert(dollar_topic.clone(), item);
                    } else {
                        for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                            if i + idx < list_items.len() {
                                interp.env_insert(p.clone(), list_items[i + idx].clone());
                            }
                        }
                        interp.env_insert(underscore.clone(), list_items[i].clone());
                        interp.env_insert(dollar_topic.clone(), list_items[i].clone());
                    }
                }
                match vm.run_reuse(&code, &compiled_fns) {
                    Ok(()) => {
                        let val = vm
                            .interpreter()
                            .env()
                            .get("_")
                            .cloned()
                            .unwrap_or(Value::Nil);
                        match val {
                            Value::Slip(elems) => result.extend(elems.iter().cloned()),
                            v => result.push(v),
                        }
                    }
                    Err(e) if e.is_next => {}
                    Err(e) if e.is_last => break,
                    Err(e) => {
                        *self = vm.into_interpreter();
                        for (k, orig) in &saved {
                            match orig {
                                Some(v) => {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                None => {
                                    self.env.remove(k);
                                }
                            }
                        }
                        return Err(e);
                    }
                }
                i += arity;
            }

            *self = vm.into_interpreter();
            // Restore original values
            for (k, orig) in saved {
                match orig {
                    Some(v) => {
                        self.env.insert(k, v);
                    }
                    None => {
                        self.env.remove(&k);
                    }
                }
            }
            return Ok(Value::array(result));
        }
        if let Some(func) = func {
            let mut result = Vec::new();
            for item in list_items {
                let value = self.call_sub_value(func.clone(), vec![item], false)?;
                match value {
                    Value::Slip(elems) => result.extend(elems.iter().cloned()),
                    v => result.push(v),
                }
            }
            return Ok(Value::array(result));
        }
        Ok(Value::array(list_items))
    }

    /// Like `eval_map_over_items` but writes back `$_` mutations to the source
    /// list elements, implementing Raku's rw binding semantics for map.
    /// Uses the same VM fast path as `eval_map_over_items` but checks for
    /// `__mutsu_rw_map_topic__` after each iteration to capture mutations.
    pub(super) fn eval_map_over_items_rw(
        &mut self,
        func: Option<Value>,
        list_items: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        let topic_key = "__mutsu_rw_map_topic__";
        if let Some(Value::Sub(data)) = func {
            let requires_full_binding = data.param_defs.iter().any(|pd| {
                pd.named
                    || pd.slurpy
                    || pd.sigilless
                    || pd.optional_marker
                    || pd.default.is_some()
                    || pd.type_constraint.is_some()
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
                    || pd.shape_constraints.is_some()
            });
            if requires_full_binding
                || data.env.contains_key("__mutsu_routine_name")
                || data.env.contains_key("__mutsu_compose_left")
            {
                // Fall through to call_sub_value path for complex cases
                let mut result = Vec::new();
                let arity = if !data.params.is_empty() {
                    let effective = data
                        .params
                        .len()
                        .saturating_sub(data.assumed_positional.len());
                    if effective == 0 { 1 } else { effective }
                } else {
                    1
                };
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    self.env.remove(topic_key);
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    if arity == 1
                        && let Some(mutated) = self.env.get(topic_key).cloned()
                    {
                        list_items[i] = mutated;
                    }
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                self.env.remove(topic_key);
                return Ok(Value::array(result));
            }

            let arity = if !data.params.is_empty() {
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration (same as eval_map_over_items)
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            let mut touched_keys: Vec<String> = Vec::with_capacity(data.params.len() + 1);
            for k in data.env.keys() {
                if !self.env.contains_key(k) {
                    touched_keys.push(k.clone());
                }
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            if !touched_keys.iter().any(|k| k == "$_") {
                touched_keys.push(dollar_topic.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            for (k, v) in &data.env {
                if !self.env.contains_key(k) {
                    self.env.insert(k.clone(), v.clone());
                }
            }

            let interp = std::mem::take(self);
            let mut vm = crate::vm::VM::new(interp);

            let mut i = 0usize;
            while i < list_items.len() {
                if arity > 1 && i + arity > list_items.len() {
                    *self = vm.into_interpreter();
                    for (k, orig) in &saved {
                        match orig {
                            Some(v) => self.env.insert(k.clone(), v.clone()),
                            None => self.env.remove(k),
                        };
                    }
                    return Err(RuntimeError::new("Not enough elements for map block arity"));
                }
                {
                    let interp = vm.interpreter_mut();
                    let assumed_count = data.assumed_positional.len();
                    for (idx, val) in data.assumed_positional.iter().enumerate() {
                        if let Some(p) = data.params.get(idx) {
                            interp.env_insert(p.clone(), val.clone());
                        }
                    }
                    // Clear the topic tracker before each iteration
                    interp.env_mut().remove(topic_key);
                    if arity == 1 {
                        let item = list_items[i].clone();
                        if let Some(p) = data.params.get(assumed_count) {
                            interp.env_insert(p.clone(), item.clone());
                        }
                        interp.env_insert(underscore.clone(), item.clone());
                        interp.env_insert(dollar_topic.clone(), item);
                    } else {
                        for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                            if i + idx < list_items.len() {
                                interp.env_insert(p.clone(), list_items[i + idx].clone());
                            }
                        }
                        interp.env_insert(underscore.clone(), list_items[i].clone());
                        interp.env_insert(dollar_topic.clone(), list_items[i].clone());
                    }
                }
                match vm.run_reuse(&code, &compiled_fns) {
                    Ok(()) => {
                        let val = vm
                            .interpreter()
                            .env()
                            .get("_")
                            .cloned()
                            .unwrap_or(Value::Nil);
                        // Write back topic mutation if it happened
                        if arity == 1
                            && let Some(mutated) = vm.interpreter().env().get(topic_key).cloned()
                        {
                            list_items[i] = mutated;
                        }
                        match val {
                            Value::Slip(elems) => result.extend(elems.iter().cloned()),
                            v => result.push(v),
                        }
                    }
                    Err(e) if e.is_next => {
                        if arity == 1
                            && let Some(mutated) = vm.interpreter().env().get(topic_key).cloned()
                        {
                            list_items[i] = mutated;
                        }
                    }
                    Err(e) if e.is_last => {
                        if arity == 1
                            && let Some(mutated) = vm.interpreter().env().get(topic_key).cloned()
                        {
                            list_items[i] = mutated;
                        }
                        break;
                    }
                    Err(e) => {
                        *self = vm.into_interpreter();
                        for (k, orig) in &saved {
                            match orig {
                                Some(v) => self.env.insert(k.clone(), v.clone()),
                                None => self.env.remove(k),
                            };
                        }
                        return Err(e);
                    }
                }
                i += arity;
            }

            *self = vm.into_interpreter();
            for (k, orig) in saved {
                match orig {
                    Some(v) => self.env.insert(k, v),
                    None => self.env.remove(&k),
                };
            }
            self.env.remove(topic_key);
            return Ok(Value::array(result));
        }
        // Non-Sub func: delegate to regular map
        self.eval_map_over_items(func, list_items.to_vec())
    }

    pub(super) fn eval_grep_over_items_with_mutated(
        &mut self,
        func: Option<Value>,
        mut list_items: Vec<Value>,
    ) -> Result<(Value, Vec<Value>), RuntimeError> {
        if let Some(Value::Sub(data)) = func {
            let mut result = Vec::new();
            let arity = if !data.params.is_empty() {
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let pred =
                        self.call_sub_value(Value::Sub(data.clone()), chunk.clone(), false)?;
                    if pred.truthy() {
                        if arity == 1 {
                            result.push(chunk[0].clone());
                        } else {
                            result.push(Value::array(chunk));
                        }
                    }
                    i += arity;
                }
                return Ok((Value::array(result), list_items));
            }

            // Compile once, reuse VM for every iteration
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            let mut touched_keys: Vec<String> = Vec::with_capacity(data.params.len() + 2);
            for k in data.env.keys() {
                if !self.env.contains_key(k) {
                    touched_keys.push(k.clone());
                }
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            if !touched_keys.iter().any(|k| k == "$_") {
                touched_keys.push(dollar_topic.clone());
            }
            let topic_source_key = "__mutsu_grep_topic_source".to_string();
            if !touched_keys.iter().any(|k| k == &topic_source_key) {
                touched_keys.push(topic_source_key.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                self.env.insert(k.clone(), v.clone());
            }

            // Create VM once, reuse across iterations
            let interp = std::mem::take(self);
            let mut vm = crate::vm::VM::new(interp);

            let mut i = 0usize;
            let mut stop = false;
            while i < list_items.len() {
                if arity > 1 && i + arity > list_items.len() {
                    break;
                }
                let chunk: Vec<Value> = if arity == 1 {
                    vec![list_items[i].clone()]
                } else {
                    list_items[i..i + arity].to_vec()
                };
                'body_redo: loop {
                    {
                        let interp = vm.interpreter_mut();
                        let assumed_count = data.assumed_positional.len();
                        for (idx, val) in data.assumed_positional.iter().enumerate() {
                            if let Some(p) = data.params.get(idx) {
                                interp.env_insert(p.clone(), val.clone());
                            }
                        }
                        if arity == 1 {
                            if let Some(p) = data.params.get(assumed_count) {
                                interp.env_insert(p.clone(), chunk[0].clone());
                            }
                            interp.env_insert(underscore.clone(), chunk[0].clone());
                            interp.env_insert(dollar_topic.clone(), chunk[0].clone());
                            interp.env_insert(topic_source_key.clone(), chunk[0].clone());
                        } else {
                            for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                                if idx < chunk.len() {
                                    interp.env_insert(p.clone(), chunk[idx].clone());
                                }
                            }
                            interp.env_insert(underscore.clone(), chunk[0].clone());
                            interp.env_insert(dollar_topic.clone(), chunk[0].clone());
                        }
                    }
                    vm.set_topic_source_var((arity == 1).then_some(topic_source_key.clone()));
                    match vm.run_reuse(&code, &compiled_fns) {
                        Ok(()) => {
                            let pred = vm
                                .interpreter()
                                .env()
                                .get("_")
                                .cloned()
                                .unwrap_or(Value::Nil);
                            let updated_item = if arity == 1 {
                                vm.interpreter()
                                    .env()
                                    .get(&topic_source_key)
                                    .cloned()
                                    .unwrap_or_else(|| chunk[0].clone())
                            } else {
                                chunk[0].clone()
                            };
                            if arity == 1 {
                                list_items[i] = updated_item.clone();
                            }
                            if pred.truthy() {
                                if arity == 1 {
                                    result.push(updated_item);
                                } else {
                                    result.push(Value::array(chunk));
                                }
                            }
                            break 'body_redo;
                        }
                        Err(e) if e.is_redo => continue 'body_redo,
                        Err(e) if e.is_next => break 'body_redo,
                        Err(e) if e.is_last => {
                            stop = true;
                            break 'body_redo;
                        }
                        Err(e) => {
                            *self = vm.into_interpreter();
                            for (k, orig) in &saved {
                                match orig {
                                    Some(v) => {
                                        self.env.insert(k.clone(), v.clone());
                                    }
                                    None => {
                                        self.env.remove(k);
                                    }
                                }
                            }
                            return Err(e);
                        }
                    }
                }
                if stop {
                    break;
                }
                i += arity;
            }

            *self = vm.into_interpreter();
            for (k, orig) in saved {
                match orig {
                    Some(v) => {
                        self.env.insert(k, v);
                    }
                    None => {
                        self.env.remove(&k);
                    }
                }
            }
            return Ok((Value::array(result), list_items));
        }
        if let Some(pattern) = func {
            if matches!(pattern, Value::Bool(_)) {
                return Err(RuntimeError::new("X::Match::Bool"));
            }
            let mut result = Vec::new();
            for item in &list_items {
                if self.smart_match(item, &pattern) {
                    result.push(item.clone());
                }
            }
            return Ok((Value::array(result), list_items));
        }
        if let Some(func) = func {
            let mut result = Vec::new();
            for item in &list_items {
                let pred = self.call_sub_value(func.clone(), vec![item.clone()], false)?;
                if pred.truthy() {
                    result.push(item.clone());
                }
            }
            return Ok((Value::array(result), list_items));
        }
        Ok((Value::array(list_items.clone()), list_items))
    }

    pub(super) fn eval_grep_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (result, _) = self.eval_grep_over_items_with_mutated(func, list_items)?;
        Ok(result)
    }

    pub(super) fn find_first_match_over_items(
        &mut self,
        func: Option<Value>,
        list_items: &[Value],
        from_end: bool,
    ) -> Result<Option<(usize, Value)>, RuntimeError> {
        if list_items.is_empty() {
            return Ok(None);
        }
        let matcher = func;
        let len = list_items.len();
        for idx in 0..len {
            let actual_idx = if from_end { len - 1 - idx } else { idx };
            let item = list_items[actual_idx].clone();
            let matched = if let Some(pattern) = &matcher {
                if matches!(pattern, Value::Sub(_)) {
                    self.call_sub_value(pattern.clone(), vec![item.clone()], true)?
                        .truthy()
                } else {
                    self.smart_match(&item, pattern)
                }
            } else {
                true
            };
            if matched {
                return Ok(Some((actual_idx, item)));
            }
        }
        Ok(None)
    }
}
