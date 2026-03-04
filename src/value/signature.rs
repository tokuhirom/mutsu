use super::Value;
use crate::ast::{Expr, ParamDef};
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Mutex;

/// Lightweight representation of a signature parameter for runtime use.
#[derive(Debug, Clone)]
pub(crate) struct SigParam {
    pub(crate) name: String,
    pub(crate) type_constraint: Option<String>,
    pub(crate) multi_invocant: bool,
    pub(crate) named: bool,
    pub(crate) slurpy: bool,
    pub(crate) double_slurpy: bool,
    pub(crate) is_capture: bool,
    pub(crate) is_invocant: bool,
    pub(crate) sigilless: bool,
    pub(crate) required: bool,
    pub(crate) has_default: bool,
    pub(crate) optional_marker: bool,
    pub(crate) sigil: char,
    pub(crate) sub_signature: Option<Vec<SigParam>>,
    pub(crate) outer_sub_signature: Option<Vec<SigParam>>,
    pub(crate) traits: Vec<String>,
    pub(crate) code_signature: Option<Box<SigInfo>>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) named_names: Vec<String>,
}

/// Complete signature info stored at runtime.
#[derive(Debug, Clone)]
pub(crate) struct SigInfo {
    pub(crate) params: Vec<SigParam>,
    pub(crate) return_type: Option<String>,
}

impl SigParam {
    fn is_optional(&self) -> bool {
        self.has_default || self.optional_marker
    }
}

// Global registry mapping Signature instance IDs to their SigInfo.
static SIG_REGISTRY: Mutex<Option<HashMap<u64, SigInfo>>> = Mutex::new(None);

fn register_sig_info(id: u64, info: SigInfo) {
    let mut guard = SIG_REGISTRY.lock().unwrap();
    let map = guard.get_or_insert_with(HashMap::new);
    map.insert(id, info);
}

/// Look up SigInfo by Instance ID.
pub(crate) fn lookup_sig_info(id: u64) -> Option<SigInfo> {
    let guard = SIG_REGISTRY.lock().unwrap();
    guard.as_ref()?.get(&id).cloned()
}

/// Convert a ParamDef (from the parser) to a SigParam (for runtime).
pub(crate) fn param_def_to_sig_param(p: &ParamDef) -> SigParam {
    let is_capture = p.slurpy && (p.name == "_capture" || p.sigilless);

    let sigil = if p.name.starts_with('@') {
        '@'
    } else if p.name.starts_with('%') {
        '%'
    } else if p.name.starts_with('&') {
        '&'
    } else {
        '$'
    };

    let name = if p.name == "_capture"
        || p.name == "__type_only__"
        || p.name.starts_with("__ANON_STATE_")
        || p.name == "__ANON_OPTIONAL__"
        || p.name == "__subsig__"
        || p.name == "__ANON_ARRAY__"
        || p.name == "__ANON_HASH__"
        || p.name == "@__ANON_ARRAY__"
        || p.name == "%__ANON_HASH__"
    {
        String::new()
    } else if p.name.starts_with('@') || p.name.starts_with('%') || p.name.starts_with('&') {
        p.name[1..].to_string()
    } else {
        p.name.clone()
    };

    let is_anon_optional = p.name == "__ANON_OPTIONAL__";
    let optional_marker =
        is_anon_optional || p.optional_marker || (!p.required && !p.slurpy && p.named);

    // For named params with alias sub-signatures like :x($a) or :x(:y(:z($a))),
    // extract named_names for introspection.
    let named_names = collect_named_names(p, &name, is_capture);

    SigParam {
        name,
        type_constraint: p.type_constraint.clone(),
        multi_invocant: p.multi_invocant,
        named: p.named,
        slurpy: p.slurpy && !is_capture,
        double_slurpy: p.double_slurpy,
        is_capture,
        is_invocant: p.is_invocant,
        sigilless: p.sigilless,
        required: p.required,
        has_default: p.default.is_some(),
        optional_marker,
        sigil,
        sub_signature: p
            .sub_signature
            .as_ref()
            .map(|subs| subs.iter().map(param_def_to_sig_param).collect()),
        outer_sub_signature: p
            .outer_sub_signature
            .as_ref()
            .map(|subs| subs.iter().map(param_def_to_sig_param).collect()),
        traits: p.traits.clone(),
        code_signature: p
            .code_signature
            .as_ref()
            .map(|(params, ret)| Box::new(param_defs_to_sig_info(params, ret.clone()))),
        where_constraint: p.where_constraint.clone(),
        named_names,
    }
}

/// For named params with alias sub-signatures (`:x($a)`, `:x(:y(:z($a)))`),
/// collect the chain of alias names as named_names.
fn collect_named_names(p: &ParamDef, name: &str, is_capture: bool) -> Vec<String> {
    if !p.named || is_capture || !matches!(&p.sub_signature, Some(subs) if subs.len() == 1) {
        return Vec::new();
    }
    let subs = p.sub_signature.as_ref().unwrap();
    let mut names = vec![name.to_string()];
    collect_named_names_recursive(&subs[0], &mut names);
    names
}

/// Recursively collect named alias names from nested `:x(:y(:z($a)))` patterns.
fn collect_named_names_recursive(pd: &ParamDef, names: &mut Vec<String>) {
    let inner_name =
        if pd.name.starts_with('@') || pd.name.starts_with('%') || pd.name.starts_with('&') {
            pd.name[1..].to_string()
        } else {
            pd.name.clone()
        };

    // If this is a named param with a single sub-signature param, keep recursing
    if pd.named
        && let Some(ref subs) = pd.sub_signature
        && subs.len() == 1
    {
        names.push(inner_name);
        collect_named_names_recursive(&subs[0], names);
    }
}

/// Convert Vec<ParamDef> and optional return type to SigInfo.
pub(crate) fn param_defs_to_sig_info(params: &[ParamDef], return_type: Option<String>) -> SigInfo {
    SigInfo {
        params: params.iter().map(param_def_to_sig_param).collect(),
        return_type,
    }
}

/// Create a Signature Value from SigInfo.
pub(crate) fn make_signature_value(info: SigInfo) -> Value {
    let raku_str = render_signature(&info);
    let mut attrs = HashMap::new();
    attrs.insert("raku".to_string(), Value::str(raku_str.clone()));
    attrs.insert("perl".to_string(), Value::str(raku_str.clone()));
    attrs.insert("Str".to_string(), Value::str(raku_str.clone()));
    attrs.insert("gist".to_string(), Value::str(raku_str));
    attrs.insert(
        "params".to_string(),
        make_params_value_from_sig_params(&info.params),
    );
    let val = Value::make_instance(Symbol::intern("Signature"), attrs);
    // Register the SigInfo for later lookup (smartmatch, etc.)
    if let Value::Instance { id, .. } = &val {
        register_sig_info(*id, info);
    }
    val
}

fn make_params_value_from_sig_params(params: &[SigParam]) -> Value {
    let values: Vec<Value> = params.iter().map(sig_param_to_parameter_instance).collect();
    Value::array(values)
}

fn sig_param_to_parameter_instance(p: &SigParam) -> Value {
    let mut attrs = HashMap::new();
    // .name returns the sigiled name (e.g., "$x", "@pos", "%named")
    // For named params with aliases (:x($a)), resolve the inner variable name
    let display_name = if !p.named_names.is_empty() {
        // Has named aliases — get innermost variable name from sub_signature
        resolve_inner_display_name(p)
    } else if p.name.is_empty() || p.is_capture || p.sigilless {
        p.name.clone()
    } else {
        format!("{}{}", p.sigil, p.name)
    };
    attrs.insert("name".to_string(), Value::str(display_name));

    // type: resolve to type object (Package) instead of string
    let type_val = match &p.type_constraint {
        Some(t) => Value::Package(Symbol::intern(t)),
        None => Value::Package(Symbol::intern("Any")),
    };
    attrs.insert("type".to_string(), type_val);

    // Slurpy hash (*%named) is considered named in Raku
    let is_named = p.named || (p.slurpy && p.sigil == '%');
    attrs.insert("named".to_string(), Value::Bool(is_named));
    attrs.insert("slurpy".to_string(), Value::Bool(p.slurpy));
    attrs.insert("sigil".to_string(), Value::str(p.sigil.to_string()));
    attrs.insert("multi-invocant".to_string(), Value::Bool(p.multi_invocant));

    // readonly: true unless rw/raw/copy/sigilless
    let is_rw = p.traits.iter().any(|t| t == "rw");
    let is_raw = p.traits.iter().any(|t| t == "raw") || p.sigilless;
    let is_copy = p.traits.iter().any(|t| t == "copy");
    let readonly = !is_rw && !is_raw && !is_copy;
    attrs.insert("readonly".to_string(), Value::Bool(readonly));
    attrs.insert("rw".to_string(), Value::Bool(is_rw));
    attrs.insert("raw".to_string(), Value::Bool(is_raw));
    attrs.insert("copy".to_string(), Value::Bool(is_copy));

    // optional
    attrs.insert("optional".to_string(), Value::Bool(p.is_optional()));

    // invocant
    attrs.insert("invocant".to_string(), Value::Bool(p.is_invocant));

    // positional: not named and not capture
    attrs.insert(
        "positional".to_string(),
        Value::Bool(!p.named && !p.is_capture),
    );

    // capture
    attrs.insert("capture".to_string(), Value::Bool(p.is_capture));

    // named_names
    let named_names_val: Vec<Value> = p
        .named_names
        .iter()
        .map(|n| Value::str(n.clone()))
        .collect();
    attrs.insert("named_names".to_string(), Value::array(named_names_val));

    // prefix: ** for double_slurpy, * for slurpy, else ""
    let prefix = if p.double_slurpy {
        "**"
    } else if p.slurpy {
        "*"
    } else {
        ""
    };
    attrs.insert("prefix".to_string(), Value::str(prefix.to_string()));

    // suffix: ? if optional positional, ! if required named, else ""
    let suffix = if p.optional_marker && !p.named && !p.slurpy {
        "?"
    } else if p.required && p.named {
        "!"
    } else {
        ""
    };
    attrs.insert("suffix".to_string(), Value::str(suffix.to_string()));

    // twigil: extract from name
    let twigil = extract_twigil(&p.name);
    attrs.insert("twigil".to_string(), Value::str(twigil.to_string()));

    // TODO: constraints - needs runtime evaluation support for where clauses
    // For now, store a marker that allows unconstrained params to smartmatch truely
    if p.where_constraint.is_none() {
        // Unconstrained param: .constraints smartmatches truely against anything
        attrs.insert("constraints".to_string(), Value::Bool(true));
    }

    if let Some(sub) = &p.sub_signature {
        attrs.insert(
            "sub-signature".to_string(),
            make_params_value_from_sig_params(sub),
        );
    }
    Value::make_instance(Symbol::intern("Parameter"), attrs)
}

/// For named params with aliases (:x($a), :x(:y(:z($a)))),
/// traverse the sub-signature chain to find the innermost variable name.
fn resolve_inner_display_name(p: &SigParam) -> String {
    fn traverse(params: &Option<Vec<SigParam>>) -> Option<String> {
        let subs = params.as_ref()?;
        if subs.len() != 1 {
            return None;
        }
        let inner = &subs[0];
        // If this inner param also has named aliases, recurse deeper
        if !inner.named_names.is_empty()
            && let Some(deeper) = traverse(&inner.sub_signature)
        {
            return Some(deeper);
        }
        // Return the sigiled name of the innermost param
        if inner.name.is_empty() || inner.is_capture || inner.sigilless {
            Some(inner.name.clone())
        } else {
            Some(format!("{}{}", inner.sigil, inner.name))
        }
    }
    traverse(&p.sub_signature).unwrap_or_else(|| format!("{}{}", p.sigil, p.name))
}

/// Extract twigil from a parameter name like `$!x` → `!`, `$.x` → `.`, `$*x` → `*`.
fn extract_twigil(name: &str) -> &str {
    // Name may start with sigil ($, @, %, &) followed by optional twigil
    let after_sigil = if name.starts_with('$')
        || name.starts_with('@')
        || name.starts_with('%')
        || name.starts_with('&')
    {
        &name[1..]
    } else {
        return "";
    };
    if after_sigil.starts_with('!') {
        "!"
    } else if after_sigil.starts_with('.') {
        "."
    } else if after_sigil.starts_with('*') {
        "*"
    } else if after_sigil.starts_with('^') {
        "^"
    } else if after_sigil.starts_with('?') {
        "?"
    } else {
        ""
    }
}

pub(crate) fn make_params_value_from_param_defs(params: &[ParamDef]) -> Value {
    let sig_params: Vec<SigParam> = params.iter().map(param_def_to_sig_param).collect();
    make_params_value_from_sig_params(&sig_params)
}

/// Extract SigInfo from a Signature Instance value.
pub(crate) fn extract_sig_info(val: &Value) -> Option<SigInfo> {
    if let Value::Instance { class_name, id, .. } = val {
        if class_name != "Signature" {
            return None;
        }
        if let Some(info) = lookup_sig_info(*id) {
            return Some(info);
        }
        // Legacy signature (no structured data)
        return Some(SigInfo {
            params: Vec::new(),
            return_type: None,
        });
    }
    None
}

/// Render a signature as Raku code.
fn render_signature(info: &SigInfo) -> String {
    if info.params.is_empty() && info.return_type.is_none() {
        return ":()".to_string();
    }
    let parts: Vec<String> = info.params.iter().map(render_param).collect();
    let params_str = parts.join(", ");
    if let Some(ref ret) = info.return_type {
        if params_str.is_empty() {
            format!(":(--> {})", ret)
        } else {
            format!(":({} --> {})", params_str, ret)
        }
    } else {
        format!(":({})", params_str)
    }
}

fn render_param(p: &SigParam) -> String {
    let mut result = String::new();

    if p.is_capture {
        result.push('|');
        if !p.name.is_empty() {
            result.push(p.sigil);
            result.push_str(&p.name);
        }
        return result;
    }

    if p.slurpy {
        result.push('*');
    }

    // Type constraint comes before the named marker: `Any :$x` not `:Any $x`
    if p.named {
        if let Some(ref tc) = p.type_constraint {
            result.push_str(tc);
            result.push(' ');
        }
        result.push(':');
    } else if let Some(ref tc) = p.type_constraint {
        result.push_str(tc);
        result.push(' ');
    }

    // Named param with alias sub-signature: :name(sub-sig)
    if p.named
        && !p.name.is_empty()
        && let Some(sub) = p.sub_signature.as_ref()
    {
        let sub_parts: Vec<String> = sub.iter().map(render_param).collect();
        result.push_str(&p.name);
        result.push('(');
        result.push_str(&sub_parts.join(", "));
        result.push(')');
        // Outer sub-signature after alias: :x($r) (Str $g, Any $i)
        if let Some(ref outer) = p.outer_sub_signature {
            let outer_parts: Vec<String> = outer.iter().map(render_param).collect();
            result.push_str(&format!(" ({})", outer_parts.join(", ")));
        }
    } else {
        result.push(p.sigil);
        if !p.name.is_empty() {
            result.push_str(&p.name);
        }

        if let Some(ref sub) = p.sub_signature {
            let sub_parts: Vec<String> = sub.iter().map(render_param).collect();
            result.push_str(&format!(" ({})", sub_parts.join(", ")));
        }
    }

    if let Some(ref cs) = p.code_signature {
        let inner = render_signature(cs);
        result.push_str(&inner);
    }

    if p.optional_marker && !p.slurpy && !p.named {
        result.push('?');
    } else if p.required && p.named {
        result.push('!');
    }

    for t in &p.traits {
        result.push_str(" is ");
        result.push_str(t);
    }

    result
}

/// Signature-Signature smartmatch: $s2 ~~ $s1
/// Returns true if $s1 ACCEPTS $s2, meaning $s1 is at least as general as $s2.
pub(crate) fn signature_smartmatch(s1: &SigInfo, s2: &SigInfo) -> bool {
    if (s1.return_type.is_some() || s2.return_type.is_some()) && s1.return_type != s2.return_type {
        return false;
    }

    // Slurpy hash (*%_) belongs to the named space even without the : prefix
    let is_named_space = |p: &&SigParam| p.named || (p.slurpy && p.sigil == '%');
    let s1_positional: Vec<&SigParam> = s1.params.iter().filter(|p| !is_named_space(p)).collect();
    let s1_named: Vec<&SigParam> = s1.params.iter().filter(|p| is_named_space(p)).collect();
    let s2_positional: Vec<&SigParam> = s2.params.iter().filter(|p| !is_named_space(p)).collect();
    let s2_named: Vec<&SigParam> = s2.params.iter().filter(|p| is_named_space(p)).collect();

    if s1_positional.iter().any(|p| p.is_capture) {
        return true;
    }

    let s1_has_slurpy_hash = s1_named.iter().any(|p| p.slurpy);
    let s1_has_slurpy_array = s1_positional.iter().any(|p| p.slurpy);

    let s1_pos_required: Vec<&&SigParam> = s1_positional
        .iter()
        .filter(|p| !p.slurpy && !p.is_optional())
        .collect();
    let s1_pos_all: Vec<&&SigParam> = s1_positional.iter().filter(|p| !p.slurpy).collect();
    let s2_pos_required: Vec<&&SigParam> = s2_positional
        .iter()
        .filter(|p| !p.slurpy && !p.is_optional())
        .collect();
    let s2_pos_all: Vec<&&SigParam> = s2_positional.iter().filter(|p| !p.slurpy).collect();

    if s2_positional.iter().any(|p| p.is_capture) && !s1_positional.iter().any(|p| p.is_capture) {
        return false;
    }

    if s2_positional.iter().any(|p| p.slurpy && p.sigil == '@')
        && !s1_has_slurpy_array
        && !s1_positional.iter().any(|p| p.is_capture)
    {
        return false;
    }

    if !s1_has_slurpy_array {
        if s2_pos_required.len() > s1_pos_all.len() {
            return false;
        }
        if s1_pos_required.len() > s2_pos_required.len() {
            return false;
        }
        if s2_pos_all.len() > s1_pos_all.len() {
            return false;
        }
    }

    let check_count = s1_pos_all.len().min(s2_pos_all.len());
    for i in 0..check_count {
        let p1 = s1_pos_all[i];
        let p2 = s2_pos_all[i];
        if !type_accepts(p1.type_constraint.as_deref(), p2.type_constraint.as_deref()) {
            return false;
        }
        if let (Some(sub1), Some(sub2)) = (&p1.sub_signature, &p2.sub_signature) {
            let sub_info1 = SigInfo {
                params: sub1.clone(),
                return_type: None,
            };
            let sub_info2 = SigInfo {
                params: sub2.clone(),
                return_type: None,
            };
            if !signature_smartmatch(&sub_info1, &sub_info2) {
                return false;
            }
        }
        if p1.sub_signature.is_some() && p2.sub_signature.is_none() {
            return false;
        }
        if let (Some(cs1), Some(cs2)) = (&p1.code_signature, &p2.code_signature)
            && !signature_smartmatch(cs1, cs2)
        {
            return false;
        }
        if p1.sigil == '@' && p2.sigil == '$' {
            return false;
        }
    }

    for i in 0..check_count {
        let p1 = s1_pos_all[i];
        let p2 = s2_pos_all[i];
        if p2.is_optional() && !p1.is_optional() && !p1.slurpy {
            return false;
        }
    }

    let s1_named_nonslurpy: Vec<&&SigParam> = s1_named.iter().filter(|p| !p.slurpy).collect();
    let s2_named_nonslurpy: Vec<&&SigParam> = s2_named.iter().filter(|p| !p.slurpy).collect();
    let s2_has_slurpy_hash = s2_named.iter().any(|p| p.slurpy);

    for p2 in &s2_named_nonslurpy {
        if s1_has_slurpy_hash {
            continue;
        }
        let found = s1_named_nonslurpy.iter().any(|p1| p1.name == p2.name);
        if !found {
            return false;
        }
    }

    if s2_has_slurpy_hash && !s1_has_slurpy_hash && !s1_positional.iter().any(|p| p.is_capture) {
        return false;
    }

    for p1 in &s1_named_nonslurpy {
        if p1.required {
            let found = s2_named_nonslurpy.iter().any(|p2| p2.name == p1.name);
            if !found && !s2_has_slurpy_hash {
                return false;
            }
        }
    }

    for p1 in &s1_named_nonslurpy {
        if let Some(p2) = s2_named_nonslurpy.iter().find(|p| p.name == p1.name) {
            if p2.required && !p1.required && !s1_has_slurpy_hash {
                return false;
            }
            if p1.required && !p2.required {
                return false;
            }
            // Compare type constraints for named params
            if !type_accepts(p1.type_constraint.as_deref(), p2.type_constraint.as_deref()) {
                return false;
            }
            // Compare outer sub-signatures for named params (e.g., :x($r) (Str $g, Any $i))
            if let (Some(os1), Some(os2)) = (&p1.outer_sub_signature, &p2.outer_sub_signature) {
                let sub_info1 = SigInfo {
                    params: os1.clone(),
                    return_type: None,
                };
                let sub_info2 = SigInfo {
                    params: os2.clone(),
                    return_type: None,
                };
                if !signature_smartmatch(&sub_info1, &sub_info2) {
                    return false;
                }
            }
            if p1.outer_sub_signature.is_some() && p2.outer_sub_signature.is_none() {
                return false;
            }
        } else if s2_has_slurpy_hash {
            // s2 doesn't have this named param — it relies on slurpy hash.
            // If s1 constrains this param's type, s1 is more restrictive than s2's catch-all.
            if !type_accepts(p1.type_constraint.as_deref(), None) {
                return false;
            }
        }
    }

    true
}

fn type_accepts(type1: Option<&str>, type2: Option<&str>) -> bool {
    match (type1, type2) {
        (None, _) => true,                             // s1 untyped (Any) accepts anything
        (Some(t1), None) => is_supertype_of(t1, "Mu"), // s1 typed, s2 untyped (Any) → s1 must accept Any
        (Some(t1), Some(t2)) => is_supertype_of(t1, t2),
    }
}

fn is_supertype_of(t1: &str, t2: &str) -> bool {
    if t1 == t2 {
        return true;
    }
    if t1 == "Mu" {
        return true;
    }
    if t1 == "Any" {
        return t2 != "Mu";
    }
    if t1 == "Cool" {
        return matches!(
            t2,
            "Str" | "Int" | "Num" | "Rat" | "FatRat" | "Bool" | "Complex"
        ) || is_supertype_of("Numeric", t2)
            || is_supertype_of("Stringy", t2);
    }
    if t1 == "Numeric" {
        return matches!(t2, "Int" | "Num" | "Rat" | "FatRat" | "Complex");
    }
    if t1 == "Real" {
        return matches!(t2, "Int" | "Num" | "Rat" | "FatRat");
    }
    if t1 == "Stringy" {
        return t2 == "Str";
    }
    false
}
