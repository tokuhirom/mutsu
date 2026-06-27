use super::*;

impl Interpreter {
    pub(super) fn dispatch_classhow_methods(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let invocant = &args[0];
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };

        // Parse named arguments
        let mut local = false;
        let mut all = false;
        let mut private = false;
        let mut tree = false;
        for arg in &args[1..] {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "local" => local = value.truthy(),
                    "all" => all = value.truthy(),
                    "private" => private = value.truthy(),
                    "tree" => tree = value.truthy(),
                    _ => {}
                }
            }
        }

        if tree {
            return self.classhow_methods_tree(&class_name, private);
        }

        let mut result = Vec::new();

        // Extract mixin role names from the invocant for runtime role method collection
        let mixin_role_names: Vec<String> = if let Value::Mixin(_, mixins) = invocant {
            mixins
                .keys()
                .filter_map(|key| key.strip_prefix("__mutsu_role__").map(String::from))
                .collect()
        } else {
            Vec::new()
        };

        if local {
            // Only methods defined directly on this class
            self.collect_class_methods(&class_name, private, &mut result);
            // Also include methods from runtime-mixed-in roles
            for role_name in &mixin_role_names {
                self.collect_role_methods(role_name, private, &mut result);
            }
        } else {
            // Walk MRO (already includes the class itself)
            let mro = self.class_mro(&class_name);

            for cn in &mro {
                if !all && (cn == "Any" || cn == "Mu") {
                    continue;
                }
                self.collect_class_methods(cn, private, &mut result);
            }

            // For built-in types that don't have class defs, add known methods
            if result.is_empty() || !self.registry().classes.contains_key(&class_name) {
                self.collect_builtin_type_methods(&class_name, &mut result);
                if all {
                    self.collect_builtin_type_methods("Any", &mut result);
                    self.collect_builtin_type_methods("Mu", &mut result);
                }
            }
        }

        Ok(Value::array(result))
    }

    pub(super) fn collect_builtin_type_methods(&self, type_name: &str, result: &mut Vec<Value>) {
        let methods: &[&str] = match type_name {
            "Str" => &[
                "chars",
                "codes",
                "comb",
                "chomp",
                "chop",
                "contains",
                "ends-with",
                "fc",
                "flip",
                "index",
                "indices",
                "lc",
                "lines",
                "match",
                "ords",
                "pred",
                "rindex",
                "samecase",
                "split",
                "starts-with",
                "substr",
                "succ",
                "tc",
                "trim",
                "trim-leading",
                "trim-trailing",
                "uc",
                "words",
                "wordcase",
                "NFC",
                "NFD",
                "NFKC",
                "NFKD",
                "encode",
                "uniparse",
                "parse-names",
                "parse-base",
                "subst",
                "subst-mutate",
                "substr-rw",
                "substr-eq",
                "trans",
                "IO",
                "Numeric",
                "Int",
                "Num",
                "Rat",
                "Bool",
                "Str",
                "gist",
                "raku",
                "elems",
                "fmt",
            ],
            "Int" | "Num" | "Rat" | "Complex" => &[
                "abs", "ceiling", "floor", "round", "sign", "sqrt", "log", "log10", "exp", "roots",
                "is-prime", "chr", "base", "polymod", "expmod", "pred", "succ", "Numeric", "Int",
                "Num", "Rat", "Bool", "Str", "gist", "raku",
            ],
            "List" | "Array" => &[
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "join",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "rotate",
                "unique",
                "repeated",
                "squish",
                "flat",
                "eager",
                "lazy",
                "head",
                "tail",
                "skip",
                "push",
                "pop",
                "shift",
                "unshift",
                "splice",
                "append",
                "prepend",
                "classify",
                "categorize",
                "min",
                "max",
                "minmax",
                "minpairs",
                "maxpairs",
                "sum",
                "pick",
                "roll",
                "permutations",
                "combinations",
                "rotor",
                "batch",
                "produce",
                "reduce",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
                "Array",
                "List",
            ],
            "Hash" => &[
                "elems",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "push",
                "append",
                "classify-list",
                "categorize-list",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Bool" => &[
                "pred", "succ", "pick", "roll", "Numeric", "Int", "Num", "Rat", "Bool", "Str",
                "gist", "raku",
            ],
            "Range" => &[
                "min",
                "max",
                "bounds",
                "elems",
                "list",
                "flat",
                "reverse",
                "pick",
                "roll",
                "sum",
                "rand",
                "minmax",
                "infinite",
                "is-int",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
                "excludes-min",
                "excludes-max",
            ],
            "Sub" | "Method" | "Block" | "Routine" | "Code" => &[
                "name",
                "signature",
                "arity",
                "count",
                "of",
                "returns",
                "Bool",
                "Str",
                "gist",
                "raku",
            ],
            "Signature" => &[
                "params", "arity", "count", "returns", "Bool", "Str", "gist", "raku",
            ],
            "IO::Path" => &[
                "absolute",
                "basename",
                "cleanup",
                "copy",
                "dirname",
                "e",
                "d",
                "f",
                "l",
                "r",
                "w",
                "x",
                "rw",
                "rwx",
                "s",
                "z",
                "extension",
                "IO",
                "lines",
                "mkdir",
                "modified",
                "accessed",
                "changed",
                "mode",
                "move",
                "open",
                "parent",
                "parts",
                "pred",
                "rename",
                "resolve",
                "rmdir",
                "sibling",
                "slurp",
                "spurt",
                "succ",
                "symlink",
                "link",
                "add",
                "child",
                "unlink",
                "volume",
                "watch",
                "words",
                "CWD",
                "SPEC",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "IO::Handle" => &[
                "open",
                "close",
                "path",
                "IO",
                "slurp",
                "spurt",
                "lines",
                "words",
                "comb",
                "split",
                "print",
                "print-nl",
                "printf",
                "say",
                "put",
                "get",
                "getc",
                "read",
                "readchars",
                "write",
                "seek",
                "tell",
                "eof",
                "flush",
                "lock",
                "unlock",
                "opened",
                "nl-in",
                "nl-out",
                "chomp",
                "encoding",
                "decode",
                "Supply",
                "native-descriptor",
                "WRITE",
                "READ",
                "t",
                "Bool",
                "Str",
                "gist",
                "raku",
            ],
            "Cool" => &[
                "substr",
                "chars",
                "codes",
                "chomp",
                "chop",
                "contains",
                "comb",
                "ends-with",
                "fc",
                "flip",
                "index",
                "indices",
                "lc",
                "lines",
                "match",
                "ords",
                "pred",
                "rindex",
                "samecase",
                "split",
                "starts-with",
                "succ",
                "tc",
                "trim",
                "trim-leading",
                "trim-trailing",
                "uc",
                "words",
                "wordcase",
                "abs",
                "ceiling",
                "floor",
                "round",
                "sign",
                "sqrt",
                "log",
                "log10",
                "exp",
                "is-prime",
                "chr",
                "base",
                "polymod",
                "Numeric",
                "Int",
                "Num",
                "Rat",
                "Bool",
                "Str",
                "gist",
                "raku",
            ],
            "Any" => &[
                "say",
                "put",
                "print",
                "note",
                "so",
                "not",
                "defined",
                "WHAT",
                "WHERE",
                "HOW",
                "WHY",
                "iterator",
                "flat",
                "eager",
                "lazy",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "unique",
                "repeated",
                "squish",
                "head",
                "tail",
                "skip",
                "min",
                "max",
                "minmax",
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "classify",
                "categorize",
                "join",
                "pick",
                "roll",
                "sum",
                "reduce",
                "produce",
                "rotor",
                "batch",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Mu" => &[
                "defined", "WHAT", "WHERE", "HOW", "WHY", "WHICH", "Bool", "Str", "gist", "raku",
                "clone", "new",
            ],
            _ => &[],
        };
        for name in methods {
            if !result.iter().any(|v| {
                if let Value::Instance { attributes, .. } = v {
                    attributes
                        .as_map()
                        .get("name")
                        .map(|n| n.to_string_value())
                        .as_deref()
                        == Some(name)
                } else {
                    false
                }
            }) {
                result.push(self.make_native_method_object(name));
            }
        }
    }
}
