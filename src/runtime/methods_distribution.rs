use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;
use std::sync::Arc;

impl Interpreter {
    /// Parse a JSON string into a Value::Hash.
    pub(crate) fn parse_json_to_value(&self, json_str: &str) -> Result<Value, RuntimeError> {
        let trimmed = json_str.trim();
        let (val, _) = parse_json_value(trimmed)
            .map_err(|e| RuntimeError::new(format!("Cannot parse JSON: {e}")))?;
        Ok(val)
    }

    /// Build the "files" hash for a distribution.
    pub(crate) fn build_dist_files_hash(&self, prefix: &str, meta: &Value) -> Value {
        let mut files = HashMap::new();
        let prefix_path = std::path::Path::new(prefix);
        if let Some(resources) = meta.hash_get_str("resources")
            && let Value::Array(arr, _) = resources
        {
            for resource in arr.iter() {
                let resource_str = resource.to_string_value();
                if resource_str.starts_with("libraries/") {
                    let lib_name = resource_str.strip_prefix("libraries/").unwrap();
                    let platform_name = platform_library_name(lib_name);
                    let actual = prefix_path
                        .join("resources")
                        .join("libraries")
                        .join(&platform_name);
                    files.insert(
                        format!("resources/{resource_str}"),
                        Value::str(actual.to_string_lossy().to_string()),
                    );
                } else {
                    let actual_path = prefix_path.join("resources").join(&resource_str);
                    files.insert(
                        format!("resources/{resource_str}"),
                        Value::str(actual_path.to_string_lossy().to_string()),
                    );
                }
            }
        }
        if let Some(provides) = meta.hash_get_str("provides")
            && let Value::Hash(map) = provides
        {
            for (_, v) in map.iter() {
                let path_str = v.to_string_value();
                files.insert(
                    path_str.clone(),
                    Value::str(prefix_path.join(&path_str).to_string_lossy().to_string()),
                );
            }
        }
        let bin_dir = prefix_path.join("bin");
        if bin_dir.is_dir()
            && let Ok(entries) = std::fs::read_dir(&bin_dir)
        {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                let path = entry.path().to_string_lossy().to_string();
                files.insert(format!("bin/{name}"), Value::str(path));
            }
        }
        Value::Hash(Arc::new(files))
    }

    /// Extract short-name and matcher fields from a depspec value.
    pub(crate) fn extract_depspec_fields(
        &self,
        depspec: &Value,
    ) -> (String, Option<String>, Option<String>, Option<String>) {
        match depspec {
            Value::CompUnitDepSpec { short_name } => (short_name.resolve(), None, None, None),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name.resolve() == "CompUnit::DependencySpecification" => {
                let sn = attributes
                    .get("short-name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes.get("auth-matcher").map(|v| v.to_string_value());
                let ver = attributes
                    .get("version-matcher")
                    .map(|v| v.to_string_value());
                let api = attributes.get("api-matcher").map(|v| v.to_string_value());
                (sn, auth, ver, api)
            }
            Value::Str(s) => (s.to_string(), None, None, None),
            _ => (String::new(), None, None, None),
        }
    }

    fn matches_depspec(
        meta: &Value,
        short_name: &str,
        auth_matcher: &Option<String>,
        version_matcher: &Option<String>,
        api_matcher: &Option<String>,
    ) -> bool {
        let meta_name = meta
            .hash_get_str("name")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if meta_name != short_name {
            return false;
        }
        if let Some(auth_m) = auth_matcher {
            let meta_auth = meta
                .hash_get_str("auth")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_auth != *auth_m {
                return false;
            }
        }
        if let Some(ver_m) = version_matcher {
            let meta_ver = meta
                .hash_get_str("ver")
                .or_else(|| meta.hash_get_str("version"))
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_ver != *ver_m {
                return false;
            }
        }
        if let Some(api_m) = api_matcher {
            let meta_api = meta
                .hash_get_str("api")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_api != *api_m {
                return false;
            }
        }
        true
    }

    /// Get candidates for a CompUnit::Repository::FileSystem.
    pub(crate) fn cur_fs_candidates(
        &self,
        prefix: &str,
        depspec: &Value,
    ) -> Result<Value, RuntimeError> {
        let (short_name, auth_matcher, version_matcher, api_matcher) =
            self.extract_depspec_fields(depspec);
        if short_name.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let prefix_path = std::path::Path::new(prefix);
        let meta_path = prefix_path.join("META6.json");
        let meta = if meta_path.exists() {
            let json_str = std::fs::read_to_string(&meta_path).map_err(|e| {
                RuntimeError::new(format!("Cannot read {}: {e}", meta_path.display()))
            })?;
            self.parse_json_to_value(&json_str)?
        } else {
            let relative = short_name.replace("::", "/");
            for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                let candidate = prefix_path.join(format!("{relative}{ext}"));
                if candidate.exists() {
                    let mut meta_map = HashMap::new();
                    meta_map.insert("name".to_string(), Value::str(short_name.clone()));
                    let mut provides_map = HashMap::new();
                    provides_map.insert(short_name.clone(), Value::str(format!("{relative}{ext}")));
                    meta_map.insert("provides".to_string(), Value::Hash(Arc::new(provides_map)));
                    let meta = Value::Hash(Arc::new(meta_map));
                    let files_hash = self.build_dist_files_hash(prefix, &meta);
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
                    attrs.insert("meta".to_string(), meta);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::array(vec![Value::make_instance(
                        crate::symbol::Symbol::intern("Distribution::Path"),
                        attrs,
                    )]));
                }
            }
            return Ok(Value::array(Vec::new()));
        };
        if !Self::matches_depspec(
            &meta,
            &short_name,
            &auth_matcher,
            &version_matcher,
            &api_matcher,
        ) {
            return Ok(Value::array(Vec::new()));
        }
        let files_hash = self.build_dist_files_hash(prefix, &meta);
        let mut attrs = HashMap::new();
        attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
        attrs.insert("meta".to_string(), meta);
        attrs.insert("files".to_string(), files_hash);
        Ok(Value::array(vec![Value::make_instance(
            crate::symbol::Symbol::intern("Distribution::Path"),
            attrs,
        )]))
    }

    /// Get candidates for a CompUnit::Repository::Installation.
    pub(crate) fn cur_inst_candidates(
        &self,
        prefix: &str,
        depspec: &Value,
    ) -> Result<Value, RuntimeError> {
        let (short_name, auth_matcher, version_matcher, api_matcher) =
            self.extract_depspec_fields(depspec);
        if short_name.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        if !dist_dir.is_dir() {
            return Ok(Value::array(Vec::new()));
        }
        let mut results = Vec::new();
        if let Ok(entries) = std::fs::read_dir(&dist_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) != Some("json") {
                    continue;
                }
                let Ok(json_str) = std::fs::read_to_string(&path) else {
                    continue;
                };
                let Ok(meta) = self.parse_json_to_value(&json_str) else {
                    continue;
                };
                if !Self::matches_depspec(
                    &meta,
                    &short_name,
                    &auth_matcher,
                    &version_matcher,
                    &api_matcher,
                ) {
                    continue;
                }
                let dist_id = path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string();
                let mut attrs = HashMap::new();
                attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
                attrs.insert("meta".to_string(), meta);
                attrs.insert("dist-id".to_string(), Value::str(dist_id));
                results.push(Value::make_instance(
                    crate::symbol::Symbol::intern("Distribution::Installation"),
                    attrs,
                ));
            }
        }
        Ok(Value::array(results))
    }

    /// Install a distribution.
    pub(crate) fn cur_inst_install(
        &self,
        prefix: &str,
        dist: &Value,
    ) -> Result<Value, RuntimeError> {
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        let sources_dir = prefix_path.join("sources");
        let resources_dir = prefix_path.join("resources");
        let bin_dir = prefix_path.join("bin");
        for dir in [&dist_dir, &sources_dir, &resources_dir, &bin_dir] {
            std::fs::create_dir_all(dir).ok();
        }
        let (meta, dist_prefix) = match dist {
            Value::Instance { attributes, .. } => {
                let m = attributes.get("meta").cloned().unwrap_or(Value::Nil);
                let p = attributes
                    .get("prefix")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                (m, p)
            }
            _ => return Ok(Value::Bool(false)),
        };
        static DIST_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let counter = DIST_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let dist_id = format!(
            "{:X}{:X}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos(),
            counter
        );
        let mut installed_provides = HashMap::new();
        if let Some(provides) = meta.hash_get_str("provides")
            && let Value::Hash(map) = provides
        {
            for (k, v) in map.iter() {
                let source_path_str = v.to_string_value();
                let source_full = std::path::Path::new(&dist_prefix).join(&source_path_str);
                let source_id = format!("{:X}", hash_strings(&[k, &dist_id]));
                let dest = sources_dir.join(&source_id);
                if source_full.exists() {
                    std::fs::copy(&source_full, &dest).ok();
                }
                let mut inner = HashMap::new();
                inner.insert("file".to_string(), Value::str(source_id));
                installed_provides.insert(k.clone(), Value::Hash(Arc::new(inner)));
            }
        }
        let mut installed_resources = HashMap::new();
        if let Some(resources_val) = meta.hash_get_str("resources")
            && let Value::Array(arr, _) = resources_val
        {
            for resource in arr.iter() {
                let resource_str = resource.to_string_value();
                let source_path = if resource_str.starts_with("libraries/") {
                    let lib_name = resource_str.strip_prefix("libraries/").unwrap();
                    let platform_name = platform_library_name(lib_name);
                    std::path::Path::new(&dist_prefix)
                        .join("resources")
                        .join("libraries")
                        .join(&platform_name)
                } else {
                    std::path::Path::new(&dist_prefix)
                        .join("resources")
                        .join(&resource_str)
                };
                let resource_id = format!("{:X}", hash_strings(&[&resource_str, &dist_id]));
                let dest = resources_dir.join(&resource_id);
                if source_path.exists() {
                    std::fs::copy(&source_path, &dest).ok();
                }
                installed_resources
                    .insert(format!("resources/{resource_str}"), Value::str(resource_id));
            }
        }
        let mut installed_bin = HashMap::new();
        let source_bin_dir = std::path::Path::new(&dist_prefix).join("bin");
        if source_bin_dir.is_dir()
            && let Ok(entries) = std::fs::read_dir(&source_bin_dir)
        {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                let bin_id = format!("{:X}", hash_strings(&[&name, &dist_id]));
                let dest = bin_dir.join(&bin_id);
                std::fs::copy(entry.path(), &dest).ok();
                installed_bin.insert(format!("bin/{name}"), Value::str(bin_id));
            }
        }
        let meta_inner = match &meta {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        let mut meta_map = match meta_inner {
            Value::Hash(map) => (**map).clone(),
            _ => HashMap::new(),
        };
        meta_map.insert(
            "provides".to_string(),
            Value::Hash(Arc::new(installed_provides)),
        );
        let mut all_files = installed_resources;
        all_files.extend(installed_bin);
        meta_map.insert("files".to_string(), Value::Hash(Arc::new(all_files)));
        meta_map.insert("dist-id".to_string(), Value::str(dist_id.clone()));
        let meta_value = Value::Hash(Arc::new(meta_map));
        let json_str = value_to_json_string(&meta_value);
        let dist_file = dist_dir.join(format!("{dist_id}.json"));
        std::fs::write(&dist_file, json_str)
            .map_err(|e| RuntimeError::new(format!("Cannot write dist file: {e}")))?;
        Ok(Value::Bool(true))
    }

    /// Uninstall a distribution.
    pub(crate) fn cur_inst_uninstall(
        &self,
        prefix: &str,
        dist: &Value,
    ) -> Result<Value, RuntimeError> {
        let dist_id = match dist {
            Value::Instance { attributes, .. } => attributes
                .get("dist-id")
                .map(|v| v.to_string_value())
                .unwrap_or_default(),
            _ => return Ok(Value::Bool(false)),
        };
        if dist_id.is_empty() {
            return Ok(Value::Bool(false));
        }
        let prefix_path = std::path::Path::new(prefix);
        let dist_file = prefix_path.join("dist").join(format!("{dist_id}.json"));
        if dist_file.exists() {
            std::fs::remove_file(&dist_file).ok();
        }
        Ok(Value::Bool(true))
    }

    /// Get installed distributions.
    pub(crate) fn cur_inst_installed(&self, prefix: &str) -> Result<Value, RuntimeError> {
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        if !dist_dir.is_dir() {
            return Ok(Value::array(Vec::new()));
        }
        let mut results = Vec::new();
        if let Ok(entries) = std::fs::read_dir(&dist_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) != Some("json") {
                    continue;
                }
                let Ok(json_str) = std::fs::read_to_string(&path) else {
                    continue;
                };
                let Ok(meta) = self.parse_json_to_value(&json_str) else {
                    continue;
                };
                let dist_id = path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string();
                let mut attrs = HashMap::new();
                attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
                attrs.insert("meta".to_string(), meta);
                attrs.insert("dist-id".to_string(), Value::str(dist_id));
                results.push(Value::make_instance(
                    crate::symbol::Symbol::intern("Distribution::Installation"),
                    attrs,
                ));
            }
        }
        Ok(Value::array(results))
    }

    /// Distribution method dispatch.
    pub(crate) fn dispatch_distribution_method(
        &self,
        _class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "prefix" => Some(Ok(attributes.get("prefix").cloned().unwrap_or(Value::Nil))),
            "meta" => Some(Ok(attributes.get("meta").cloned().unwrap_or(Value::Nil))),
            "content" => {
                let path_arg = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let prefix = attributes
                    .get("prefix")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let full_path = std::path::Path::new(&prefix).join(&path_arg);
                Some(Ok(self.make_io_path_instance(&full_path.to_string_lossy())))
            }
            "Str" | "gist" => {
                let name = attributes
                    .get("meta")
                    .and_then(|m| m.hash_get_str("name"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "Distribution".to_string());
                Some(Ok(Value::str(name)))
            }
            _ => None,
        }
    }

    /// CUR::Installation method dispatch.
    pub(crate) fn dispatch_cur_installation_method(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let prefix = attributes
            .get("prefix")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        match method {
            "candidates" => {
                let depspec = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_candidates(&prefix, &depspec))
            }
            "install" => {
                let dist = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_install(&prefix, &dist))
            }
            "uninstall" => {
                let dist = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_uninstall(&prefix, &dist))
            }
            "installed" => Some(self.cur_inst_installed(&prefix)),
            "path-spec" => Some(Ok(Value::str(format!("inst#{prefix}")))),
            "resolve" => {
                let depspec = args.first().cloned().unwrap_or(Value::Nil);
                match self.cur_inst_candidates(&prefix, &depspec) {
                    Ok(Value::Array(arr, _)) => {
                        if arr.is_empty() {
                            Some(Ok(Value::Nil))
                        } else {
                            Some(Ok(Value::Bool(true)))
                        }
                    }
                    Ok(_) => Some(Ok(Value::Nil)),
                    Err(e) => Some(Err(e)),
                }
            }
            "files" => {
                let search_path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let prefix_path = std::path::Path::new(&prefix);
                let dist_dir = prefix_path.join("dist");
                let mut results = Vec::new();
                if dist_dir.is_dir()
                    && let Ok(entries) = std::fs::read_dir(&dist_dir)
                {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.extension().and_then(|e| e.to_str()) != Some("json") {
                            continue;
                        }
                        let Ok(json_str) = std::fs::read_to_string(&path) else {
                            continue;
                        };
                        let Ok(meta) = self.parse_json_to_value(&json_str) else {
                            continue;
                        };
                        if let Some(files) = meta.hash_get_str("files")
                            && files.hash_get_str(&search_path).is_some()
                        {
                            let dist_id = path
                                .file_stem()
                                .unwrap_or_default()
                                .to_string_lossy()
                                .to_string();
                            let mut attrs = HashMap::new();
                            attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                            attrs.insert("meta".to_string(), meta);
                            attrs.insert("dist-id".to_string(), Value::str(dist_id));
                            results.push(Value::make_instance(
                                crate::symbol::Symbol::intern("Distribution::Installation"),
                                attrs,
                            ));
                        }
                    }
                }
                Some(Ok(Value::array(results)))
            }
            _ => None,
        }
    }
}

// ---- Simple JSON parser (no serde_json dependency) ----

fn parse_json_value(s: &str) -> Result<(Value, &str), String> {
    let s = s.trim_start();
    if s.is_empty() {
        return Err("Unexpected end of JSON".to_string());
    }
    match s.as_bytes()[0] {
        b'{' => parse_json_object(&s[1..]),
        b'[' => parse_json_array(&s[1..]),
        b'"' => parse_json_string(&s[1..]),
        b't' if s.starts_with("true") => Ok((Value::Bool(true), &s[4..])),
        b'f' if s.starts_with("false") => Ok((Value::Bool(false), &s[5..])),
        b'n' if s.starts_with("null") => Ok((Value::Nil, &s[4..])),
        b'-' | b'0'..=b'9' => parse_json_number(s),
        ch => Err(format!("Unexpected character in JSON: {}", ch as char)),
    }
}

fn parse_json_object(s: &str) -> Result<(Value, &str), String> {
    let mut map = HashMap::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix('}') {
        return Ok((Value::Hash(Arc::new(map)), rest));
    }
    loop {
        let s2 = s.trim_start();
        if !s2.starts_with('"') {
            return Err("Expected string key in JSON object".to_string());
        }
        let (key_val, rest) = parse_json_string(&s2[1..])?;
        let key = key_val.to_string_value();
        let rest = rest.trim_start();
        if !rest.starts_with(':') {
            return Err("Expected ':' in JSON object".to_string());
        }
        let (val, rest) = parse_json_value(&rest[1..])?;
        map.insert(key, val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix('}') {
            return Ok((Value::Hash(Arc::new(map)), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after;
        } else {
            return Err("Expected ',' or '}' in JSON object".to_string());
        }
    }
}

fn parse_json_array(s: &str) -> Result<(Value, &str), String> {
    let mut items = Vec::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix(']') {
        return Ok((Value::array(items), rest));
    }
    loop {
        let (val, rest) = parse_json_value(s)?;
        items.push(val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix(']') {
            return Ok((Value::array(items), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after.trim_start();
        } else {
            return Err("Expected ',' or ']' in JSON array".to_string());
        }
    }
}

fn parse_json_string(s: &str) -> Result<(Value, &str), String> {
    let mut result = String::new();
    let mut chars = s.chars();
    loop {
        match chars.next() {
            None => return Err("Unterminated JSON string".to_string()),
            Some('"') => {
                return Ok((Value::str(result), chars.as_str()));
            }
            Some('\\') => match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(cp) = u32::from_str_radix(&hex, 16)
                        && let Some(ch) = char::from_u32(cp)
                    {
                        result.push(ch);
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => return Err("Unterminated escape in JSON string".to_string()),
            },
            Some(c) => result.push(c),
        }
    }
}

fn parse_json_number(s: &str) -> Result<(Value, &str), String> {
    let mut end = 0;
    let bytes = s.as_bytes();
    if end < bytes.len() && bytes[end] == b'-' {
        end += 1;
    }
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    let mut is_float = false;
    if end < bytes.len() && bytes[end] == b'.' {
        is_float = true;
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        is_float = true;
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    let num_str = &s[..end];
    if is_float {
        let f: f64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Num(f), &s[end..]))
    } else {
        let i: i64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Int(i), &s[end..]))
    }
}

fn value_to_json_string(val: &Value) -> String {
    match val {
        Value::Hash(map) => {
            let parts: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("  {:?}: {}", k, value_to_json_string(v)))
                .collect();
            format!("{{\n{}\n}}", parts.join(",\n"))
        }
        Value::Array(arr, _) => {
            let parts: Vec<String> = arr.iter().map(value_to_json_string).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Str(s) => format!("{:?}", s.to_string()),
        Value::Int(i) => format!("{i}"),
        Value::Bool(b) => format!("{b}"),
        Value::Nil => "null".to_string(),
        _ => format!("{:?}", val.to_string_value()),
    }
}

fn platform_library_name(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("lib{name}.dylib")
    } else if cfg!(target_os = "windows") {
        format!("{name}.dll")
    } else {
        format!("lib{name}.so")
    }
}

fn hash_strings(strings: &[&str]) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for s in strings {
        s.hash(&mut hasher);
    }
    hasher.finish()
}
