//! The default `MAIN` usage message: `$*USAGE` and what a failed `MAIN`
//! dispatch prints when no `GENERATE-USAGE` is provided.
//!
//! A port of Rakudo's `default-generate-usage` (`src/core.c/Main.rakumod`):
//! one line per candidate of the dispatched routine, in declaration order,
//! with named options first (unless `%*SUB-MAIN-OPTS<named-anywhere>`),
//! followed by a table of the parameters documented with `#=` / `#|` and
//! their defaults.

use super::*;
use crate::ast::{FunctionDef, ParamDef};
use crate::value::signature::{SigParam, param_def_to_sig_param};
use std::sync::Arc;

/// One routine candidate a usage message describes.
pub(super) struct UsageCandidate {
    pub(super) def: Arc<FunctionDef>,
    /// The key the candidate's own declarator doc is filed under
    /// (`&MAIN`, or `&MAIN/multi.N` for a multi candidate).
    doc_key: String,
}

/// Longest default value shown verbatim in the documentation table.
const MAX_DEFAULT_CHARS: usize = 20;

impl Interpreter {
    /// The candidates of the routine `package`/`name` a usage message lists:
    /// every declared candidate in declaration order, minus those declared
    /// `is hidden-from-USAGE`. Falls back to `fallback` when the routine has
    /// no registered candidate (a def known only through its value).
    // Cost: O(f log f), f = registered functions (see `routine_candidate_defs`).
    pub(super) fn usage_candidates(
        &self,
        package: &str,
        name: &str,
        fallback: Option<&FunctionDef>,
    ) -> Vec<UsageCandidate> {
        let defs = self.routine_candidate_defs(package, name);
        if defs.is_empty() {
            return fallback
                .map(|def| UsageCandidate {
                    def: Arc::new(def.clone()),
                    doc_key: format!("&{}", def.name),
                })
                .into_iter()
                .collect();
        }
        defs.into_iter()
            .enumerate()
            .filter(|(_, (def, _))| {
                !self
                    .main_hidden_from_usage
                    .contains(&def.body_fingerprint())
            })
            .map(|(idx, (def, is_multi))| {
                let doc_key = if is_multi {
                    format!("&{}/multi.{idx}", def.name)
                } else {
                    format!("&{}", def.name)
                };
                UsageCandidate { def, doc_key }
            })
            .collect()
    }

    /// Build the usage message for `candidates`. With `first_positional`
    /// (the first positional argument of the failed dispatch), candidates
    /// whose first parameter is a literal accepting it are preferred, so a
    /// sub-command's usage shows only that sub-command.
    // Cost: O(c * p), c = candidates, p = parameters per candidate (plus
    // evaluating the default of each documented parameter).
    pub(super) fn generate_usage(
        &mut self,
        candidates: &[UsageCandidate],
        first_positional: Option<&Value>,
    ) -> String {
        let named_anywhere = self.read_sub_main_opts().named_anywhere;
        let prog_name = self.usage_program_name();
        let selected: Vec<&UsageCandidate> = match first_positional {
            Some(first) => {
                let matching: Vec<&UsageCandidate> = candidates
                    .iter()
                    .filter(|c| {
                        c.def
                            .param_defs
                            .first()
                            .and_then(|pd| pd.literal_value.as_ref())
                            .is_some_and(|lit| {
                                let lit = lit.clone();
                                self.usage_literal_accepts(&lit, first)
                            })
                    })
                    .collect();
                if matching.is_empty() {
                    candidates.iter().collect()
                } else {
                    matching
                }
            }
            None => candidates.iter().collect(),
        };

        let mut help_msgs: Vec<String> = Vec::new();
        let mut arg_help: Vec<(String, String)> = Vec::new();
        for candidate in selected {
            let mut required_named = Vec::new();
            let mut optional_named = Vec::new();
            let mut positional = Vec::new();
            for pd in &candidate.def.param_defs {
                let sp = param_def_to_sig_param(pd);
                if sp.is_capture || sp.is_invocant {
                    continue;
                }
                let Some((argument, help_key, kind)) = self.usage_argument(pd, &sp) else {
                    continue;
                };
                if let Some(why) = self.usage_param_doc(&candidate.def, pd, &sp)
                    && !arg_help.iter().any(|(k, _)| *k == help_key)
                {
                    arg_help.push((help_key, why));
                }
                match kind {
                    ArgKind::RequiredNamed => required_named.push(argument),
                    ArgKind::OptionalNamed => optional_named.push(argument),
                    ArgKind::Positional => positional.push(argument),
                }
            }
            let docs = self
                .doc_comments
                .get(&candidate.doc_key)
                .map(|doc| format!("-- {}", doc.doc.contents()));
            let mut parts = vec![prog_name.clone()];
            if named_anywhere {
                parts.extend(positional);
                parts.extend(required_named);
                parts.extend(optional_named);
            } else {
                parts.extend(required_named);
                parts.extend(optional_named);
                parts.extend(positional);
            }
            parts.extend(docs);
            help_msgs.push(parts.join(" "));
        }

        if !arg_help.is_empty() {
            help_msgs.push(String::new());
            let offset = arg_help
                .iter()
                .map(|(k, _)| k.chars().count())
                .max()
                .unwrap_or(0)
                + 4;
            for (key, why) in &arg_help {
                let pad = " ".repeat(offset - key.chars().count());
                help_msgs.push(format!("  {key}{pad}{why}"));
            }
        }

        if help_msgs.is_empty() {
            "No usage information could be determined".to_string()
        } else {
            let lines: Vec<String> = help_msgs.iter().map(|l| format!("  {l}")).collect();
            format!("Usage:\n{}", lines.join("\n"))
        }
    }

    /// The program name a usage line starts with: `-e '...'` for a one-liner,
    /// the bare name for a script found through `PATH`, else `$*PROGRAM-NAME`
    /// as given. `%*ENV<PERL6_PROGRAM_NAME>` overrides it.
    fn usage_program_name(&self) -> String {
        let name = std::env::var("PERL6_PROGRAM_NAME")
            .ok()
            .filter(|s| !s.is_empty())
            .or_else(|| {
                self.env
                    .get("*PROGRAM-NAME")
                    .or_else(|| self.env.get("$*PROGRAM-NAME"))
                    .map(|v| v.to_string_value())
            })
            .unwrap_or_else(|| "program".to_string());
        if name == "-e" {
            "-e '...'".to_string()
        } else {
            strip_path_prefix(&name)
        }
    }

    /// Whether the literal `lit` (a parameter's value constraint) accepts the
    /// command-line argument `arg` (`lit.ACCEPTS(arg)`).
    fn usage_literal_accepts(&self, lit: &Value, arg: &Value) -> bool {
        match lit.view() {
            ValueView::Str(s) => s.to_string() == arg.to_string_value(),
            _ => {
                let arg = arg.to_string_value();
                match arg.trim().parse::<f64>() {
                    Ok(b) if lit.is_numeric() => lit.to_f64() == b,
                    _ => lit.to_string_value() == arg,
                }
            }
        }
    }

    /// How parameter `pd` is shown on a usage line, how its row in the
    /// documentation table is labelled (an optional named option without its
    /// brackets), and which group of the line it belongs to. `None` for a
    /// parameter that is not shown (an anonymous `*%`).
    fn usage_argument(
        &mut self,
        pd: &ParamDef,
        sp: &SigParam,
    ) -> Option<(String, String, ArgKind)> {
        let ty = self.usage_param_type(sp);
        let (constraints, literals, total) = usage_constraints(pd, sp, &ty);
        // A bare literal parameter (`'add'`) carries a placeholder name.
        let anonymous = sp.name.is_empty() || sp.name == "__literal__";
        if sp.double_slurpy || (sp.slurpy && sp.sigil == '%') {
            // A named slurpy: `*%h` collects every other option.
            let argument = format!("--<{}>=...", sp.name);
            return (!anonymous)
                .then(|| (format!("[{argument}]"), argument, ArgKind::OptionalNamed));
        }
        if sp.named {
            let mut names: Vec<String> = sp.named_names.iter().rev().cloned().collect();
            if names.is_empty() {
                names.push(sp.name.clone());
            }
            let mut argument = names
                .iter()
                .map(|n| {
                    if n.chars().count() == 1 {
                        format!("-{n}")
                    } else {
                        format!("--{n}")
                    }
                })
                .collect::<Vec<_>>()
                .join("|");
            let type_name = usage_type_name(sp, &ty);
            if sp.sigil == '@' {
                let shown = if constraints.is_empty() {
                    "Any"
                } else {
                    &constraints
                };
                argument.push_str(&format!("=<{shown}> ..."));
            } else if type_name != "Bool" {
                let shown = if constraints.is_empty() {
                    type_name.clone()
                } else {
                    constraints.clone()
                };
                if self.usage_type_accepts_true(pd, sp, &ty) {
                    argument.push_str(&format!("[={shown}]"));
                } else {
                    argument.push_str(&format!("=<{shown}>"));
                }
                if let Some(options) = self.usage_enum_options(&type_name) {
                    if options.chars().count() > 50 {
                        let cut: String = options.chars().take(50).collect();
                        argument.push_str(&format!(" ({cut}..."));
                    } else {
                        argument.push_str(&format!(" ({options})"));
                    }
                }
            }
            return Some(if pd.required {
                (argument.clone(), argument, ArgKind::RequiredNamed)
            } else {
                (format!("[{argument}]"), argument, ArgKind::OptionalNamed)
            });
        }
        let mut argument = if !anonymous {
            format!("<{}>", sp.name)
        } else if !constraints.is_empty() {
            if literals == total {
                constraints.clone()
            } else {
                format!("<{constraints}>")
            }
        } else {
            format!("<{}>", usage_type_name(sp, &ty))
        };
        if sp.slurpy {
            argument = format!("[{argument} ...]");
        } else if pd.optional_marker || pd.default.is_some() {
            argument = format!("[{argument}]");
        }
        if total > 0 && literals == total {
            if argument.contains('\'') {
                argument = argument.replace('\'', "'\"'\"'");
            }
            if argument.contains([' ', '"']) {
                argument = format!("'{argument}'");
            }
        }
        Some((argument.clone(), argument, ArgKind::Positional))
    }

    /// Split a parameter's declared type the way Rakudo's `Parameter` does:
    /// a subset type contributes its nominal base as `.type` and itself as a
    /// constraint (`S $x` has type `Any` and constraint `S`).
    fn usage_param_type(&self, sp: &SigParam) -> ParamType {
        let Some(declared) = sp.type_constraint.clone() else {
            return ParamType::default();
        };
        let registry = self.registry();
        let Some(mut subset) = registry.subsets.get(declared.as_str()) else {
            return ParamType {
                nominal: Some(declared),
                subset: None,
            };
        };
        // A subset of a subset: walk to the first non-subset base.
        let mut nominal = subset.base.clone();
        for _ in 0..32 {
            match registry.subsets.get(nominal.as_str()) {
                Some(next) => {
                    subset = next;
                    nominal = subset.base.clone();
                }
                None => break,
            }
        }
        ParamType {
            nominal: Some(nominal),
            subset: Some(declared),
        }
    }

    /// Whether a named option's type (or one of its constraints) accepts
    /// `True`, which makes its value optional on the command line (`[=Int]`).
    fn usage_type_accepts_true(&mut self, pd: &ParamDef, sp: &SigParam, ty: &ParamType) -> bool {
        if matches!(sp.sigil, '%' | '&') {
            return false;
        }
        let nominal_accepts = match &ty.nominal {
            None => true,
            Some(tc) => self.type_matches_value(tc, &Value::TRUE),
        };
        // Rakudo also counts any constraint that accepts `True`. A `where`
        // clause is not run here: it only narrows a nominal type, and the
        // nominal types that reject `True` (`Str`, `Num`, ...) are rarely
        // narrowed to accept it.
        nominal_accepts
            || ty
                .subset
                .clone()
                .is_some_and(|subset| self.type_matches_value(&subset, &Value::TRUE))
            || pd
                .literal_value
                .as_ref()
                .is_some_and(|lit| matches!(lit.view(), ValueView::Bool(true)))
    }

    /// The sorted value names of the enum `type_name`, space-separated.
    fn usage_enum_options(&self, type_name: &str) -> Option<String> {
        let key = self.resolve_enum_type_key(type_name)?;
        let registry = self.registry();
        let variants = registry.enum_types.get(key.as_str())?;
        let mut names: Vec<&str> = variants.iter().map(|(k, _)| k.as_str()).collect();
        names.sort_unstable();
        Some(names.join(" "))
    }

    /// The documentation-table text of a parameter: its `#=`/`#|` comment,
    /// plus `[default: ...]` when it has a defined default value.
    fn usage_param_doc(
        &mut self,
        def: &FunctionDef,
        pd: &ParamDef,
        sp: &SigParam,
    ) -> Option<String> {
        let key = format!("&{}::{}{}", def.name, sp.sigil, sp.name);
        let mut why = self.doc_comments.get(&key)?.doc.contents();
        if let Some(default_expr) = &pd.default
            && let Ok(value) = self.eval_param_default_expr(pd, default_expr)
            && crate::runtime::types::value_is_defined(&value)
        {
            let quote = matches!(value.view(), ValueView::Str(_));
            let middle = matches!(value.view(), ValueView::Int(_) | ValueView::BigInt(_));
            let mut shown = value.to_string_value();
            let chars = shown.chars().count();
            if chars > MAX_DEFAULT_CHARS {
                shown = if middle {
                    let half = MAX_DEFAULT_CHARS / 2;
                    let head: String = shown.chars().take(half - 1).collect();
                    let tail: String = shown.chars().skip(chars - half).collect();
                    format!("{head}…{tail}")
                } else {
                    let head: String = shown.chars().take(MAX_DEFAULT_CHARS - 1).collect();
                    format!("{head}…")
                };
            }
            if quote {
                shown = format!("'{shown}'");
            }
            why.push_str(&format!(" [default: {shown}]"));
        }
        Some(why)
    }
}

/// A parameter's type as Rakudo's `Parameter` reports it (see
/// [`Interpreter::usage_param_type`]).
#[derive(Default)]
struct ParamType {
    /// `.type`: the nominal type; `None` for an untyped parameter.
    nominal: Option<String>,
    /// The subset type, which Rakudo lists among the constraints.
    subset: Option<String>,
}

/// Which group of a usage line an argument belongs to.
enum ArgKind {
    RequiredNamed,
    OptionalNamed,
    Positional,
}

/// A parameter's post-constraints as a usage string, with how many of them
/// are literal values and how many there are in all: a literal shows as its
/// gist, a `where` clause as `where { ... }` (prefixed by the type when it is
/// the only constraint).
fn usage_constraints(pd: &ParamDef, sp: &SigParam, ty: &ParamType) -> (String, usize, usize) {
    let mut parts: Vec<String> = Vec::new();
    let mut literals = 0;
    let mut total = 0;
    if let Some(subset) = &ty.subset {
        total += 1;
        parts.push(subset.clone());
    }
    if let Some(lit) = &pd.literal_value {
        total += 1;
        literals += 1;
        parts.push(lit.to_string_value());
    }
    if pd.where_constraint.is_some() {
        total += 1;
        parts.push("where { ... }".to_string());
    }
    parts.dedup();
    let mut constraints = parts.join(" ");
    if constraints == "where { ... }" {
        constraints = format!("{} {constraints}", usage_type_name(sp, ty));
    }
    (constraints, literals, total)
}

/// `$param.type.^name`: the declared type, or the sigil's default container
/// type (`Positional[Int]` for `Int @a`).
fn usage_type_name(sp: &SigParam, ty: &ParamType) -> String {
    let declared = ty.nominal.as_deref();
    let role = match sp.sigil {
        '@' => "Positional",
        '%' => "Associative",
        '&' => "Callable",
        _ => return declared.unwrap_or("Any").to_string(),
    };
    match declared {
        Some(t) => format!("{role}[{t}]"),
        None => role.to_string(),
    }
}

/// Rakudo's `strip_path_prefix`: a script run through `PATH` is named by its
/// base name (or by its installed wrapper's name), unless an earlier `PATH`
/// entry shadows it; any other name is kept as given.
fn strip_path_prefix(name: &str) -> String {
    use std::path::Path;
    let path = Path::new(name);
    let Some(base) = path.file_name().and_then(|b| b.to_str()) else {
        return name.to_string();
    };
    let dir = path.parent().map(Path::to_path_buf).unwrap_or_default();
    let canon = |p: &Path| std::fs::canonicalize(p).ok();
    let dir_canon = canon(if dir.as_os_str().is_empty() {
        Path::new(".")
    } else {
        &dir
    });
    let Some(path_var) = std::env::var_os("PATH") else {
        return name.to_string();
    };
    let elems: Vec<std::path::PathBuf> = std::env::split_paths(&path_var).collect();
    for elem in &elems {
        let file = elem.join(base);
        if is_executable_file(&file) {
            return if canon(elem) == dir_canon {
                base.to_string()
            } else {
                name.to_string()
            };
        }
    }
    if let Some(wrapper_base) = base.strip_suffix(".raku") {
        for elem in &elems {
            let wrapper = elem.join(wrapper_base);
            if is_executable_file(&wrapper) && elem.join(base).is_file() {
                return if canon(elem) == dir_canon {
                    wrapper_base.to_string()
                } else {
                    name.to_string()
                };
            }
        }
    }
    name.to_string()
}

fn is_executable_file(path: &std::path::Path) -> bool {
    let Ok(meta) = std::fs::metadata(path) else {
        return false;
    };
    if !meta.is_file() {
        return false;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        meta.permissions().mode() & 0o111 != 0
    }
    #[cfg(not(unix))]
    {
        true
    }
}
