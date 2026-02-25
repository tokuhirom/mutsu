use super::Value;
use crate::ast::ParamDef;
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
    pub(crate) is_capture: bool,
    pub(crate) required: bool,
    pub(crate) has_default: bool,
    pub(crate) optional_marker: bool,
    pub(crate) sigil: char,
    pub(crate) sub_signature: Option<Vec<SigParam>>,
    pub(crate) outer_sub_signature: Option<Vec<SigParam>>,
    pub(crate) traits: Vec<String>,
    pub(crate) code_signature: Option<Box<SigInfo>>,
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
    let is_capture = p.slurpy && p.name == "_capture";

    let sigil = if p.name.starts_with('@') {
        '@'
    } else if p.name.starts_with('%') {
        '%'
    } else if p.name.starts_with('&') {
        '&'
    } else {
        '$'
    };

    let name = if is_capture
        || p.name == "__type_only__"
        || p.name == "__ANON_STATE__"
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

    SigParam {
        name,
        type_constraint: p.type_constraint.clone(),
        multi_invocant: p.multi_invocant,
        named: p.named,
        slurpy: p.slurpy && !is_capture,
        is_capture,
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
    attrs.insert("raku".to_string(), Value::Str(raku_str.clone()));
    attrs.insert("perl".to_string(), Value::Str(raku_str.clone()));
    attrs.insert("Str".to_string(), Value::Str(raku_str.clone()));
    attrs.insert("gist".to_string(), Value::Str(raku_str));
    attrs.insert(
        "params".to_string(),
        make_params_value_from_sig_params(&info.params),
    );
    let val = Value::make_instance("Signature".to_string(), attrs);
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
    attrs.insert("name".to_string(), Value::Str(p.name.clone()));
    attrs.insert(
        "type".to_string(),
        p.type_constraint
            .as_ref()
            .map(|t| Value::Str(t.clone()))
            .unwrap_or(Value::Nil),
    );
    attrs.insert("named".to_string(), Value::Bool(p.named));
    attrs.insert("slurpy".to_string(), Value::Bool(p.slurpy));
    attrs.insert("sigil".to_string(), Value::Str(p.sigil.to_string()));
    attrs.insert("multi-invocant".to_string(), Value::Bool(p.multi_invocant));
    if let Some(sub) = &p.sub_signature {
        attrs.insert(
            "sub-signature".to_string(),
            make_params_value_from_sig_params(sub),
        );
    }
    Value::make_instance("Parameter".to_string(), attrs)
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
