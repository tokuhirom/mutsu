//! The wrapped instance's own methods on a role mixin.
//!
//! `Foo.new but role { ... }` is a `Mixin` value whose inner value is the
//! `Foo` instance. The native fast path (`try_native_method`) already declines
//! a method the mixed-in roles declare (`mixin_role_has_method`), but it
//! answered every other name out of its receiver-blind cascades -- so an
//! accessor of the wrapped class that shares its name with an `Any` method
//! (`has IO() $.cache`, `has $.list`, `has $.elems`) returned the builtin's
//! answer (`Any.cache` is `(self,)`) instead of the attribute. zef's plugin
//! loader mixes a `$.short-name` role into every plugin, and
//! `Zef::Repository::LocalCache.store`'s `$.cache.IO.child(...)` then copied
//! each fetched distribution into a directory named after the instance's gist
//! in the current directory (#10232).
use super::*;

impl Interpreter {
    /// Whether `target` is a role mixin over a user instance whose class
    /// declares `method` itself, as an explicit method or a public accessor.
    // Cost: O(d), d = the wrapped class's MRO depth (two MRO probes).
    pub(crate) fn mixin_wrapped_instance_has_method(
        &mut self,
        target: &Value,
        method: &str,
    ) -> bool {
        // A named receiver (`$o.cache`, the `CallMethodMut` path) arrives
        // still in its variable's container.
        let decont;
        let target = if target.is_container_ref() {
            decont = target.deref_container();
            &decont
        } else {
            target
        };
        // Tag probe first -- a `view()` on a lazy Match would materialize it.
        if !target.is_mixin_value() {
            return false;
        }
        let ValueView::Mixin(inner, _) = target.view() else {
            return false;
        };
        let ValueView::Instance { class_name, .. } = inner.view() else {
            return false;
        };
        let class_name = class_name.resolve();
        self.has_user_method(&class_name, method) || self.has_public_accessor(&class_name, method)
    }
}
