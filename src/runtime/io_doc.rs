use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// Install the declarator documentation the parser attached to this
    /// compilation unit's declarations (ADR-0134): `doc_comment_list` in
    /// source order for `$=pod`, and `doc_comments` keyed the way `.WHY`
    /// looks a named declaration up. An anonymous routine or block has no
    /// name to be found by -- its code object carries its documentation (see
    /// `crate::decl_doc::DeclDoc`) -- so it is listed for `$=pod` only.
    pub(super) fn install_doc_comments(&mut self, docs: Vec<DocComment>) {
        self.why_cache.clear();
        self.why_object_cache.clear();
        self.doc_comments = docs
            .iter()
            .filter(|dc| !dc.is_anonymous)
            .map(|dc| (dc.key.clone(), dc.clone()))
            .collect();
        self.doc_comment_list = docs;
    }

    /// Add Pod::Block::Declarator entries to $=pod from doc_comment_list.
    pub(super) fn add_declarator_pod_entries(&mut self, declarants: &ValueMap) {
        use super::DocDeclKind;
        // Get existing $=pod entries
        let mut pod_entries: Vec<Value> =
            if let Some(ValueView::Array(arr, _)) = self.env.get("=pod").map(Value::view) {
                arr.iter().cloned().collect()
            } else {
                Vec::new()
            };
        // Add declarator doc entries, preferring the concrete declarant built
        // from the AST: DOC INIT runs before the program's registration
        // opcodes, but Pod::To::Text needs both `.WHY` identity and the
        // routine's real signature at that point.
        for dc in &self.doc_comment_list {
            // The key first: it is what distinguishes the candidates of a
            // multi (`&mm/multi.0` vs `&mm/multi.1`) and one routine's `$a`
            // from another's. `wherefore_name` is the fallback for a
            // declaration whose key is simply its name.
            let wherefore = (!dc.is_anonymous)
                .then(|| {
                    declarants
                        .get(&dc.key)
                        .or_else(|| declarants.get(&dc.wherefore_name))
                        .cloned()
                })
                .flatten()
                .unwrap_or_else(|| match dc.kind {
                    DocDeclKind::Package => {
                        Value::package(crate::symbol::Symbol::intern(&dc.wherefore_name))
                    }
                    DocDeclKind::Block => Value::package(crate::symbol::Symbol::intern("Block")),
                    DocDeclKind::Sub => {
                        // Use callable_type_override if set (Method, Submethod).
                        // A proto handle's .^name is "Sub" too (Rakudo; "Routine"
                        // is never a concrete value's type), so no proto case.
                        let base_type = if let Some(ref ct) = dc.callable_type_override {
                            ct.as_str()
                        } else {
                            "Sub"
                        };
                        // For subs with return types (e.g., "anon Str sub {}"),
                        // produce "Sub+{Callable[Str]}" format. Only a Sub:
                        // rakudo leaves a Method/Submethod's name alone.
                        let type_name = match dc.return_type {
                            Some(ref rt) if base_type == "Sub" => {
                                format!("{}+{{Callable[{}]}}", base_type, rt)
                            }
                            _ => base_type.to_string(),
                        };
                        Value::package(crate::symbol::Symbol::intern(&type_name))
                    }
                    DocDeclKind::GrammarRule => {
                        Value::package(crate::symbol::Symbol::intern("Regex"))
                    }
                    DocDeclKind::Attr => Value::package(crate::symbol::Symbol::intern("Attribute")),
                    DocDeclKind::Param => {
                        Value::package(crate::symbol::Symbol::intern("Parameter"))
                    }
                });
            let object_id = match wherefore.view() {
                ValueView::Sub(data) => Some(data.id),
                ValueView::WeakSub(data) => data.upgrade().map(|data| data.id),
                ValueView::Instance { id, .. } => Some(id),
                _ => None,
            };
            let pod_entry = Interpreter::make_pod_declarator(&dc.doc, wherefore);
            if let Some(object_id) = object_id {
                self.why_object_cache.insert(object_id, pod_entry.clone());
            }
            pod_entries.push(pod_entry);
        }
        self.env
            .insert("=pod".to_string(), Value::real_array(pod_entries));
    }
}
