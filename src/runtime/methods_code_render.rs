//! The rendering of a code object, `gist` and `raku` (ADR-12523 slice 7).
//!
//! Rakudo declares `gist` and `raku` on `Routine` (and `raku` on `Block`), not
//! on `Code`; the recognition table folds those owners into `Code`, so they
//! are no `Code` rows. The by-name arms of `dispatch_routine_method` and
//! `dispatch_sub_method` call these routines.

use super::*;
use crate::value::signature::{make_signature_value, param_defs_to_sig_info};

impl Interpreter {
    /// `gist` / `raku` of a `&name` handle on a routine reached through the
    /// registry (`ValueView::Routine`).
    // Cost: O(p) to render a signature of p parameters; a multi dispatcher's
    // proto form is O(c * p) over its c candidates.
    pub(super) fn routine_handle_render(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if method == "gist" {
            // Rakudo: a Sub handle gists as `&name` and a builtin-method
            // lookup handle (package = owning type) as the bare name.
            let display = super::methods_instance_ops::format_operator_name(name);
            let is_method = !(crate::qualified::is_global_name(package) || package.is_empty());
            if !is_method {
                return Some(Ok(Value::str(format!("&{}", display))));
            }
            return Some(Ok(Value::str(display)));
        }
        if matches!(method, "raku") {
            // A routine with `multi` candidates is its dispatcher, which
            // Rakudo spells as its proto: `proto sub hs (|) {*}`.
            if let Some(raku) = self.dispatcher_raku(package, name) {
                return Some(Ok(Value::str(raku)));
            }
            // TODO: Rakudo renders a builtin routine's .raku as its proto
            // (`proto sub say (|) {*}`); mutsu has no real protos for
            // builtins, so the long candidate-signature form remains.
            // For Routine handles, render using the first candidate's signature
            let candidates = self.routine_candidate_subs(package, name);
            let sig_gist = if !candidates.is_empty() {
                if let Ok(sig) =
                    self.call_method_with_values(candidates[0].clone(), "signature", Vec::new())
                    && let ValueView::Instance { attributes, .. } = sig.view()
                {
                    attributes
                        .as_map()
                        .get("gist")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "()".to_string())
                } else {
                    "()".to_string()
                }
            } else {
                let (params, param_defs) = self.callable_signature(target);
                let defs = if !param_defs.is_empty() {
                    param_defs
                } else {
                    params
                        .into_iter()
                        .map(|name| ParamDef { trait_meta: Default::default(),
                            type_capture: None,
                            name,
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            named_alias: false,
                            slurpy: false,
                            double_slurpy: false,
                            onearg: false,
                            sigilless: false,
                            type_constraint: None,
                            literal_value: None,
                            sub_signature: None,
                            where_constraint: None,
                            traits: Vec::new(),
                            optional_marker: false,
                            outer_sub_signature: None,
                            code_signature: None,
                            is_invocant: false,
                            shape_constraints: None,
                            block_param: false,
                            code: Default::default(),
                            trait_args: Vec::new(),
                        })
                        .collect()
                };
                let info = param_defs_to_sig_info(&defs, None);
                let sig = make_signature_value(info, Some(&*self));
                if let ValueView::Instance { attributes, .. } = sig.view() {
                    attributes
                        .as_map()
                        .get("gist")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "()".to_string())
                } else {
                    "()".to_string()
                }
            };
            return Some(Ok(Value::str(format!(
                "sub {} {} {{ #`(Sub|0) ... }}",
                name, sig_gist
            ))));
        }
        None
    }

    /// `gist` / `raku` of a `Sub` value (a routine, closure or block).
    // Cost: O(p) to render a signature of p parameters; a multi dispatcher's
    // proto form is O(c * p) over its c candidates.
    pub(super) fn sub_render(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // The dispatcher `&name` of a `multi sub` (a Sub carrying its captured
        // candidates) is spelled as its proto, like the by-name handle above.
        if matches!(method, "raku")
           
            && data.env.contains_key("__mutsu_multi_dispatch_candidates")
            && !matches!(
                data.env.get("__mutsu_callable_type").map(Value::view),
                Some(ValueView::Str(kind)) if matches!(kind.as_str(), "Method" | "Submethod")
            )
            && let Some(raku) = self.dispatcher_raku(&data.package.resolve(), &data.name.resolve())
        {
            return Some(Ok(Value::str(raku)));
        }
        // `.Str` is the `Code.Str` row's routine (`method_table::code`).
        if method == "Str" {
            return self.code_str(target);
        }
        if matches!(method, "raku" | "gist") {
            let sig = self.sub_signature_value(data);
            let sig_gist = if let ValueView::Instance { attributes, .. } = sig.view() {
                attributes
                    .as_map()
                    .get("gist")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "()".to_string())
            } else {
                "()".to_string()
            };
            let name = data.name.resolve();
            let id = data.id;
            let is_anon = name.is_empty() || name == "<anon>";
            // Bare/pointy blocks are `Block`s: `-> sig { #`(Block|id) ... }`
            // for gist/Str/raku alike (an anon *sub* is a `Sub` even without
            // a name, so blockness is decided by is_bare_block, not the name).
            if data.is_bare_block {
                // Rakudo writes a Block's signature BARE after the arrow --
                // `-> $a { ... }`, not `-> ($a) { ... }` -- where `.raku` of a
                // Sub keeps the parens (`sub ($a) { ... }`). `sig_gist` is the
                // Signature's own gist, which is parenthesized, so strip one
                // layer for this form only.
                let sig_bare = sig_gist
                    .strip_prefix('(')
                    .and_then(|s| s.strip_suffix(')'))
                    .unwrap_or(&sig_gist);
                return Some(Ok(Value::str(format!(
                    "-> {} {{ #`(Block|{}) ... }}",
                    sig_bare, id
                ))));
            }
            if method == "gist" {
                // Rakudo: a named Sub gists as `&name`; an anonymous sub as
                // `sub { }` (the signature is not shown). A Method/Submethod
                // gists as its BARE name instead (`C.^lookup('m').gist` is
                // `m`) -- reached here by the method-typed closure values that
                // carry the `__mutsu_callable_type` marker rather than being a
                // real method `Instance`, such as the `WHEREFORE` of a
                // `Pod::Block::Declarator`.
                if is_anon {
                    return Some(Ok(Value::str_from("sub { }")));
                }
                if matches!(
                    data.env.get("__mutsu_callable_type").map(Value::view),
                    Some(ValueView::Str(kind)) if matches!(kind.as_str(), "Method" | "Submethod")
                ) {
                    return Some(Ok(Value::str(name)));
                }
                return Some(Ok(Value::str(format!("&{}", name))));
            }
            // .raku / .perl — an empty signature is omitted entirely:
            // `sub foo { ... }`, `sub ($x) { ... }`, `sub { ... }`.
            let sig_part = if sig_gist == "()" {
                String::new()
            } else {
                format!("{} ", sig_gist)
            };
            if is_anon {
                return Some(Ok(Value::str(format!(
                    "sub {}{{ #`(Sub|{}) ... }}",
                    sig_part, id
                ))));
            }
            // A `multi` candidate (an entry of a dispatcher's `.candidates`)
            // is spelled `multi sub`, as Rakudo does.
            let keyword = if data.env.contains_key("__mutsu_is_multi_candidate") {
                "multi sub"
            } else {
                "sub"
            };
            return Some(Ok(Value::str(format!(
                "{} {} {}{{ #`(Sub|{}) ... }}",
                keyword, name, sig_part, id
            ))));
        }
        None
    }
}
