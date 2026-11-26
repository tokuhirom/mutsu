//! The attribute-bearing rows of the static class catalogue (ADR-19206):
//! `Proc`, `IO::Handle`, `IO::CatHandle`/`IO::ArgFiles`, the `Systemic`
//! classes (`Distro`, `Kernel`, `VM`, `Compiler`, `Raku`/`Perl`),
//! `Encoding::Builtin`, the Pod object model and the IO notification classes.
//!
//! [`build_all`] is the code `build_builtin_registry` used to run for them at
//! every start-up, moved verbatim. It now runs once per process, the first
//! time any of these classes is looked up, and the catalogue hands out each
//! class from its result. They are built together because some read others
//! (`IO::ArgFiles` is a clone of `IO::CatHandle`) and the code was written as
//! one sequence; together they cost a fraction of a millisecond, once.
//!
//! Their accessor rows are not written into the method table: a catalogue
//! class answers `Registry::accessor_is_public_sym` from its own attribute
//! list.

use std::sync::OnceLock;

use super::*;
use crate::value::ValueMap;

/// The classes [`build_all`] registers, one `name<TAB><TAB>` row each, sorted
/// by name (the catalogue checks this at compile time). A unit test keeps this
/// list and [`build_all`] in step.
pub(super) const ATTR_CLASS_ROWS: &str = concat!(
    "Compiler\t\t\n",
    "Distro\t\t\n",
    "Encoding::Builtin\t\t\n",
    "IO::ArgFiles\t\t\n",
    "IO::CatHandle\t\t\n",
    "IO::Handle\t\t\n",
    "IO::Notification\t\t\n",
    "IO::Notification::Change\t\t\n",
    "Kernel\t\t\n",
    "Perl\t\t\n",
    "Pod::Block\t\t\n",
    "Pod::Block::Code\t\t\n",
    "Pod::Block::Comment\t\t\n",
    "Pod::Block::Declarator\t\t\n",
    "Pod::Block::Named\t\t\n",
    "Pod::Block::Para\t\t\n",
    "Pod::Block::Table\t\t\n",
    "Pod::Config\t\t\n",
    "Pod::Defn\t\t\n",
    "Pod::FormattingCode\t\t\n",
    "Pod::Heading\t\t\n",
    "Pod::Item\t\t\n",
    "Proc\t\t\n",
    "Proc::Async::Pipe\t\t\n",
    "Raku\t\t\n",
    "VM\t\t\n",
);

/// The class `name`, from the one-time build of this group.
// Cost: O(s), s = the class's size (a clone); the first call in the process
// also builds the whole group.
pub(super) fn build_attr_class(name: &str) -> ClassDef {
    static ALL: OnceLock<rustc_hash::FxHashMap<String, ClassDef>> = OnceLock::new();
    ALL.get_or_init(build_all)
        .get(name)
        .cloned()
        .unwrap_or_default()
}

/// Every class of this group, built as `build_builtin_registry` built them.
// Cost: O(c), c = the classes' total size.
fn build_all() -> rustc_hash::FxHashMap<String, ClassDef> {
    let mut classes: rustc_hash::FxHashMap<String, ClassDef> = rustc_hash::FxHashMap::default();
    // `Proc::Async.stdout` / `.stderr` (#16667): rakudo's core `Supply`
    // subclass. The instance is a supply underneath, answered by the
    // `Supply` rows (`DispatchShape::Supply`, `value::supply_class`).
    classes.insert(
        crate::value::supply_class::PROC_ASYNC_PIPE.to_string(),
        ClassDef {
            parents: vec!["Supply".to_string()],
            attributes: Default::default(),
            native_methods: HashSet::new(),
            mro: sym_mro(&[crate::value::supply_class::PROC_ASYNC_PIPE, "Supply"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    classes.insert("Proc".to_string(), {
        // Proc's public attributes, registered so `.raku`/`.gist` renders
        // the full Rakudo form. An un-run `Proc.new` seeds each with its
        // type-object / value default: `in`/`out`/`err` => IO::Pipe,
        // os-error => Str, signal => Any, exitcode/pid => Nil, command => [].
        let proc_attr = |name: &str, sigil: char, default: Option<Expr>| -> ClassAttributeDef {
            ClassAttributeDef {
                name: name.to_string(),
                is_public: true,
                default: default.map(|e| crate::opcode::DeclTraitArg::Ast(Box::new(e))),
                captured_env: None,
                captured_unit: None,
                declaring_package: None,
                is_rw: false,
                is_required: None,
                sigil,
                type_constraint: None,
                where_constraint: None,
                declared_shape: None,
                source_line: None,
                source_file: None,
                default_is_seed: false,
                default_is_trait: false,
            }
        };
        let nil_default = || Some(Expr::Literal(Value::NIL));
        let mut attribute_types = HashMap::new();
        attribute_types.insert("in".to_string(), "IO::Pipe".to_string());
        attribute_types.insert("out".to_string(), "IO::Pipe".to_string());
        attribute_types.insert("err".to_string(), "IO::Pipe".to_string());
        attribute_types.insert("os-error".to_string(), "Str".to_string());
        ClassDef {
            parents: Vec::new(),
            attributes: From::from(vec![
                proc_attr("in", '$', None),
                proc_attr("out", '$', None),
                proc_attr("err", '$', None),
                proc_attr("os-error", '$', None),
                proc_attr("exitcode", '$', nil_default()),
                proc_attr("signal", '$', None),
                proc_attr("pid", '$', nil_default()),
                proc_attr("command", '@', None),
            ]),
            // `Str`/`gist` are NOT native: Rakudo's Proc has no stringifier
            // of its own, so the default instance repr applies (`Proc.new`),
            // not the exitcode number the old native arm rendered.
            native_methods: crate::builtins::method_table::handle_names("Proc")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["Proc"]),
            attribute_types,
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        }
    });
    classes.insert(
        "IO::Handle".to_string(),
        ClassDef {
            parents: Vec::new(),
            // Rakudo seeds $!path with the IO type object. Keeping this as
            // an attribute default also covers IO::Handle.bless and derived
            // classes, instead of only patching the rendered gist.
            attributes: From::from(vec![ClassAttributeDef {
                name: "path".to_string(),
                is_public: true,
                default: Some(crate::opcode::DeclTraitArg::Ast(Box::new(Expr::Literal(
                    Value::package(Symbol::intern("IO")),
                )))),
                captured_env: None,
                captured_unit: None,
                declaring_package: None,
                is_rw: false,
                is_required: None,
                sigil: '$',
                type_constraint: None,
                where_constraint: None,
                declared_shape: None,
                source_line: None,
                source_file: None,
                default_is_seed: false,
                default_is_trait: false,
            }]),
            native_methods: [
                "path",
                "IO",
                "Str",
                "gist",
                "DESTROY",
                "close",
                "get",
                "getc",
                "readchars",
                "lines",
                "words",
                "read",
                "write",
                "print",
                "say",
                "put",
                "flush",
                "seek",
                "tell",
                "eof",
                "encoding",
                "opened",
                "t",
                "lock",
                "unlock",
                "spurt",
                "slurp",
                // Deprecated Rakudo alias for `.slurp` from the current
                // position (META6's `multi method new(IO::Handle :$file!)`).
                "slurp-rest",
                "out-buffer",
                "Supply",
                "open",
                "nl-out",
                "nl-in",
                "print-nl",
                "native-descriptor",
            ]
            .iter()
            .map(|s| s.to_string())
            .collect(),
            mro: sym_mro(&["IO::Handle"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    classes.insert(
        "IO::CatHandle".to_string(),
        ClassDef {
            // `IO::CatHandle is IO::Handle` (rakudo): a cat satisfies
            // `IO::Handle` constraints and smartmatches (#17754).
            parents: vec!["IO::Handle".to_string()],
            attributes: Default::default(),
            native_methods: [
                "get",
                "getc",
                "lines",
                "words",
                "comb",
                "split",
                "slurp",
                "readchars",
                "read",
                "next-handle",
                "open",
                "close",
                "DESTROY",
                "eof",
                "opened",
                "chomp",
                "nl-in",
                "encoding",
                "on-switch",
                "path",
                "IO",
                "handles",
                "Str",
                "gist",
                "Supply",
                "native-descriptor",
                "seek",
                "tell",
                "t",
                "lock",
                "unlock",
                "raku",
                "perl",
                // Write/low-level methods are NYI on a read-only cat; the
                // native handler raises X::NYI for them (rakudo does too).
                "flush",
                "out-buffer",
                "print",
                "printf",
                "print-nl",
                "put",
                "say",
                "write",
                "WRITE",
                "READ",
                "EOF",
                // Obsolete: `.slurp-rest` -> `.slurp`.
                "slurp-rest",
            ]
            .iter()
            .map(|s| s.to_string())
            .collect(),
            mro: sym_mro(&["IO::CatHandle", "IO::Handle"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    // `IO::ArgFiles` is a cat handle over the program's arguments
    // (`$*ARGFILES`, `IO::ArgFiles.new(@files)`): `IO::CatHandle`'s methods,
    // with its own name first in the MRO (#17754).
    if let Some(cat) = classes.get("IO::CatHandle").cloned() {
        let mut argfiles = cat;
        argfiles.parents = vec!["IO::CatHandle".to_string()];
        argfiles.mro = sym_mro(&["IO::ArgFiles", "IO::CatHandle", "IO::Handle"]);
        classes.insert("IO::ArgFiles".to_string(), argfiles);
    }
    classes.insert(
        "Distro".to_string(),
        ClassDef {
            parents: Vec::new(),
            attributes: systemic_attributes(&["release", "path-sep", "name", "auth", "version", "signature", "desc"]),
            native_methods: crate::builtins::method_table::systemic_names("Distro")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["Distro"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    // The compiler-identity object is `Raku` (`$*RAKU.^name` is `Raku`, and
    // `Raku.^name` is `Raku` in rakudo). `Perl` is the pre-rename spelling,
    // which rakudo still keeps as a distinct type name (`Perl.^name` is
    // `Perl`) and still answers `$*PERL` with; both share one native method
    // table, and `make_perl_instance` builds the `Raku` one.
    for identity_name in ["Raku", "Perl"] {
        classes.insert(
            identity_name.to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: systemic_attributes(&["compiler", "name", "auth", "version", "signature", "desc"]),
                native_methods: crate::builtins::method_table::systemic_names(identity_name)
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&[identity_name]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
    }
    classes.insert(
        "Kernel".to_string(),
        ClassDef {
            parents: Vec::new(),
            // Its attributes: a type object refuses their accessors with
            // "Cannot look up attributes" (#13952).
            // In Rakudo's order, which `Mu.raku` renders; `auth` is read by
            // its row from the instance and is not listed there.
            attributes: systemic_attributes(&[
                "name", "version", "hardware", "arch", "bits", "release", "desc",
                "signature",
            ]),
            native_methods: crate::builtins::method_table::systemic_names("Kernel")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["Kernel"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    classes.insert(
        "VM".to_string(),
        ClassDef {
            parents: Vec::new(),
            attributes: systemic_attributes(&["config", "prefix", "precomp-ext", "precomp-target", "name", "auth", "version", "signature", "desc"]),
            native_methods: crate::builtins::method_table::systemic_names("VM")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["VM"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    classes.insert(
        "Compiler".to_string(),
        ClassDef {
            parents: Vec::new(),
            attributes: systemic_attributes(&["id", "release", "codename", "name", "auth", "version", "signature", "desc"]),
            native_methods: crate::builtins::method_table::systemic_names("Compiler")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["Compiler"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    classes.insert(
        "Encoding::Builtin".to_string(),
        ClassDef {
            parents: Vec::new(),
            attributes: systemic_attributes(&["name"]),
            native_methods: crate::builtins::method_table::encoding_names("Encoding::Builtin")
                .map(str::to_string)
                .collect(),
            mro: sym_mro(&["Encoding::Builtin"]),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: ValueMap::default(),
        },
    );
    // Pod object model (rakudo `src/core.c/Pod.pm6`). The declared
    // attribute list is what `.raku`/`.gist` enumerate, so it must carry
    // the same names, sigils and ORDER rakudo emits -- e.g.
    // `Pod::Heading.new(level => 1, config => {}, contents => [...])`.
    // `collect_class_attributes_display_order` walks the MRO forwards
    // (most-derived first) and keeps a redeclared name at its most-derived
    // position, so each subclass listing its own attributes before
    // `config`/`contents` reproduces rakudo's order exactly.
    // A `%`/`@` sigil also keeps the rendered value un-itemized
    // (`config => {}`, not `config => ${}`).
    /// `(class name, parent names, declared `(attribute, sigil)` pairs)`.
    type PodClassDecl = (
        &'static str,
        &'static [&'static str],
        &'static [(&'static str, char)],
    );
    const POD_CLASSES: &[PodClassDecl] = &[
        ("Pod::Block", &[], &[("config", '%'), ("contents", '@')]),
        (
            "Pod::Block::Code",
            &["Pod::Block"],
            &[("config", '%'), ("contents", '@')],
        ),
        (
            "Pod::Block::Comment",
            &["Pod::Block"],
            &[("config", '%'), ("contents", '@')],
        ),
        // `leading`/`trailing` are `$`-sigil here because that is what the
        // accessors return in rakudo too (`.leading` is the joined text,
        // not the private `@!leading` buffer), and `contents` likewise
        // holds the newline-joined doc text the `.WHY` consumers compare
        // against (roast S26 `is $thing.WHY.contents, "$leading\n$trailing"`).
        (
            "Pod::Block::Declarator",
            &["Pod::Block"],
            &[
                ("WHEREFORE", '$'),
                ("leading", '$'),
                ("trailing", '$'),
                ("config", '%'),
                ("contents", '$'),
            ],
        ),
        (
            "Pod::Block::Named",
            &["Pod::Block"],
            &[("name", '$'), ("config", '%'), ("contents", '@')],
        ),
        (
            "Pod::Block::Para",
            &["Pod::Block"],
            &[("config", '%'), ("contents", '@')],
        ),
        (
            "Pod::Block::Table",
            &["Pod::Block"],
            &[
                ("caption", '$'),
                ("headers", '@'),
                ("config", '%'),
                ("contents", '@'),
            ],
        ),
        ("Pod::Config", &[], &[("type", '$'), ("config", '%')]),
        (
            "Pod::Defn",
            &["Pod::Block"],
            &[("term", '$'), ("config", '%'), ("contents", '@')],
        ),
        (
            "Pod::FormattingCode",
            &["Pod::Block"],
            &[
                ("type", '$'),
                ("meta", '@'),
                ("config", '%'),
                ("contents", '@'),
            ],
        ),
        (
            "Pod::Heading",
            &["Pod::Block"],
            &[("level", '$'), ("config", '%'), ("contents", '@')],
        ),
        (
            "Pod::Item",
            &["Pod::Block"],
            &[("level", '$'), ("config", '%'), ("contents", '@')],
        ),
    ];
    for (name, parents, attrs) in POD_CLASSES {
        let mut mro_names: Vec<&str> = vec![name];
        mro_names.extend_from_slice(parents);
        classes.insert(
            (*name).to_string(),
            ClassDef {
                parents: parents.iter().map(|p| (*p).to_string()).collect(),
                attributes: attrs
                    .iter()
                    .map(|(attr, sigil)| ClassAttributeDef {
                        name: (*attr).to_string(),
                        is_public: true,
                        default: None,
                        captured_env: None,
                        captured_unit: None,
                        declaring_package: None,
                        is_rw: false,
                        is_required: None,
                        sigil: *sigil,
                        type_constraint: None,
                        where_constraint: None,
                        declared_shape: None,
                        source_line: None,
                        source_file: None,
                        default_is_seed: false,
                        default_is_trait: false,
                    })
                    .collect(),
                native_methods: HashSet::new(),
                mro: sym_mro(&mro_names),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
    }

    for (name, class_def) in Interpreter::io_notification_class_defs() {
        classes.insert(name, class_def);
    }
    classes
}

/// The attributes of a `Systemic` class (`Distro`, `VM`, `Raku`, `Compiler`),
/// in Rakudo's order, so `Mu.raku` renders them as Rakudo does
/// (`Distro.new(release => "24.04", ...)`); the methods that read them are
/// rows (ADR-11276 §9.155).
fn systemic_attributes(names: &[&str]) -> super::class_attributes::ClassAttributes {
    names
        .iter()
        .map(|name| ClassAttributeDef {
            name: (*name).to_string(),
            is_public: true,
            default: None,
            captured_env: None,
            captured_unit: None,
            declaring_package: None,
            is_rw: false,
            is_required: None,
            sigil: '$',
            type_constraint: None,
            where_constraint: None,
            declared_shape: None,
            source_line: None,
            source_file: None,
            default_is_seed: false,
            default_is_trait: false,
        })
        .collect::<Vec<_>>()
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The row list names exactly the classes [`build_all`] builds.
    #[test]
    fn rows_name_exactly_the_built_classes() {
        let mut built: Vec<String> = build_all().into_keys().collect();
        built.sort();
        let mut rows: Vec<String> = ATTR_CLASS_ROWS
            .lines()
            .map(|row| row.split('\t').next().unwrap_or_default().to_string())
            .collect();
        rows.sort();
        assert_eq!(rows, built);
    }
}
