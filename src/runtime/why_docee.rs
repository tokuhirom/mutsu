//! `Attribute.WHY`'s docee binding.
//!
//! Rakudo's `Attribute.WHY` is `$!why.set_docee(self); $!why`: it returns the
//! very `Pod::Block::Declarator` that sits in `$=pod`, and makes that block's
//! `WHEREFORE` the attribute it was called on. A pod walker comparing
//! `$=pod[$i].WHEREFORE` against `Foo.^attributes[0]` (Pod::TreeWalker's
//! `t/declarators.rakutest`) relies on both halves.

use super::*;

impl Interpreter {
    /// The `$=pod` declarator block documenting the attribute `target`
    /// (owner `owner`, full name `name` such as `$!foo`), re-pointed at
    /// `target` and remembered under its identity so later `.WHY` calls on it
    /// are a cache hit. `None` when the attribute has no declarator block.
    // Cost: O(d) on the first call per attribute object, d = declarator
    // blocks in the compilation unit; O(1) afterwards (why_object_cache).
    pub(super) fn attribute_why_set_docee(
        &mut self,
        target: &Value,
        owner: &str,
        name: &str,
    ) -> Option<Value> {
        let ValueView::Instance { id, .. } = target.view() else {
            return None;
        };
        let pod = self
            .declarator_docs.why_object_cache
            .values()
            .find(|pod| Self::declarator_documents_attribute(pod, owner, name))
            .cloned()?;
        if let ValueView::Instance { attributes, .. } = pod.view() {
            attributes.insert("WHEREFORE", target.clone());
        }
        self.declarator_docs.why_object_cache.insert(id, pod.clone());
        Some(pod)
    }

    fn declarator_documents_attribute(pod: &Value, owner: &str, name: &str) -> bool {
        let ValueView::Instance { attributes, .. } = pod.view() else {
            return false;
        };
        let Some(wherefore) = attributes.as_map().get("WHEREFORE").cloned() else {
            return false;
        };
        let ValueView::Instance {
            class_name,
            attributes: attr,
            ..
        } = wherefore.view()
        else {
            return false;
        };
        if class_name != "Attribute" {
            return false;
        }
        let attr = attr.as_map();
        let str_of = |key: &str| attr.get(key).map(Value::to_string_value);
        str_of("name").as_deref() == Some(name)
            && str_of("__mutsu_attr_owner").as_deref() == Some(owner)
    }
}
