use std::collections::BTreeMap;
use crate::ast::{ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_stmt};

#[derive(Default)]
struct S(BTreeMap<String, usize>);
impl S {
    fn hit(&mut self, k: &str) {
        *self.0.entry(k.to_string()).or_default() += 1;
    }
    fn params(&mut self, kind: &str, p: &[ParamDef]) {
        for p in p {
            if p.default.is_some() { self.hit(&format!("{kind}: param default")); }
            if p.where_constraint.is_some() { self.hit(&format!("{kind}: param where")); }
            if p.sub_signature.is_some() { self.hit(&format!("{kind}: param sub_signature")); }
            if p.outer_sub_signature.is_some() { self.hit(&format!("{kind}: param outer_sub_sig")); }
            if p.code_signature.is_some() { self.hit(&format!("{kind}: param code_sig")); }
            if p.shape_constraints.is_some() { self.hit(&format!("{kind}: param shape")); }
            if p.literal_value.is_some() { self.hit(&format!("{kind}: param literal")); }
            if p.type_capture.is_some() { self.hit(&format!("{kind}: param type_capture")); }
            if !p.no_traits() { self.hit(&format!("{kind}: param traits")); }
            if !p.trait_args.is_empty() { self.hit(&format!("{kind}: param trait_args")); }
        }
    }
}
impl<'ast> Visit<'ast> for S {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Some(r) = super::pure::reason_of(stmt) { self.hit(&format!("REASON {r}")); }
        match stmt {
            Stmt::SubDecl { name_expr, associativity, precedence_trait, return_type, signature_alternates, export_tags, custom_traits, is_export, is_test_assertion, supersede, is_hidden_from_backtrace, param_defs, .. } => {
                self.hit("Sub: total");
                if name_expr.is_some() { self.hit("Sub: name_expr"); }
                if associativity.is_some() { self.hit("Sub: assoc"); }
                if precedence_trait.is_some() { self.hit("Sub: precedence"); }
                if return_type.is_some() { self.hit("Sub: return_type"); }
                if !signature_alternates.is_empty() { self.hit("Sub: alternates"); }
                if !export_tags.is_empty() { self.hit("Sub: export_tags"); }
                if !custom_traits.is_empty() { self.hit("Sub: custom_traits"); }
                if *is_export { self.hit("Sub: is_export"); }
                if *is_test_assertion { self.hit("Sub: test_assertion"); }
                if *supersede { self.hit("Sub: supersede"); }
                if *is_hidden_from_backtrace { self.hit("Sub: hidden"); }
                self.params("Sub", param_defs);
            }
            Stmt::MethodDecl { name_expr, value_prototype, return_type, deprecated_message, handles, custom_traits, is_export, is_hidden_from_backtrace, export_tags, nested_capture_index, param_defs, .. } => {
                self.hit("Method: total");
                if name_expr.is_some() { self.hit("Method: name_expr"); }
                if value_prototype.is_some() { self.hit("Method: value_prototype"); }
                if return_type.is_some() { self.hit("Method: return_type"); }
                if deprecated_message.is_some() { self.hit("Method: deprecated"); }
                if !handles.is_empty() { self.hit("Method: handles"); }
                if !custom_traits.is_empty() { self.hit("Method: custom_traits"); }
                if *is_export { self.hit("Method: is_export"); }
                if *is_hidden_from_backtrace { self.hit("Method: hidden"); }
                if !export_tags.is_empty() { self.hit("Method: export_tags"); }
                if nested_capture_index.is_some() { self.hit("Method: nested_capture"); }
                self.params("Method", param_defs);
            }
            _ => {}
        }
        walk_stmt(self, stmt);
    }
}

#[test]
#[ignore]
fn survey_tmp() {
    let Ok(list) = std::env::var("ORACLE_FILES") else { return };
    let run = move || {
        let mut s = S::default();
        for path in std::fs::read_to_string(list).unwrap().lines() {
            let Ok(src) = std::fs::read_to_string(path) else { continue };
            let Ok((st, _)) = crate::parse_dispatch::parse_source_spelled(&src) else { continue };
            for x in &st { s.visit_stmt(x); }
        }
        for (k, v) in s.0 { println!("{k}\t{v}"); }
    };
    std::thread::Builder::new().stack_size(512 << 20).spawn(run).unwrap().join().unwrap();
}

#[test]
#[ignore]
fn dump_spelled_tmp() {
    let src = std::env::var("DUMP_SRC").unwrap();
    let run = move || {
        let (st, _) = crate::parse_dispatch::parse_source_spelled(&src).unwrap();
        for s in &st {
            println!("{s:?}");
        }
    };
    std::thread::Builder::new().stack_size(256 << 20).spawn(run).unwrap().join().unwrap();
}
