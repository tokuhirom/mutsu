//! `Compiler` native methods that are not shared with the `Raku`/`Perl`
//! compiler-identity object — chiefly `.verbose-config`.

use crate::runtime::*;
use crate::value::AttrMap;

impl Interpreter {
    /// `$*RAKU.compiler` native methods. Everything the identity object
    /// (`Raku`/`Perl`) also answers is delegated to `native_perl`; the extras
    /// here are the ones only a `Compiler` has.
    pub(in crate::runtime) fn native_compiler(
        &self,
        attributes: &AttrMap,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "verbose-config" => Ok(Self::compiler_verbose_config()),
            _ => Ok(self.native_perl(attributes, method)),
        }
    }

    /// `Compiler.verbose-config` — a map of section name to a map of
    /// configuration key/value pairs, the shape rakudo returns.
    ///
    /// Rakudo's sections describe the MoarVM build that produced the running
    /// binary (`moar::cc`, `moar::ldflags`, third-party library paths, ...).
    /// mutsu has no such build database and must not invent one, so it reports
    /// only what is actually true of this interpreter and the host it is
    /// running on: its own identity/build facts, and the kernel/distro facts
    /// already backing `$*KERNEL` / `$*DISTRO`. The structure matches rakudo's
    /// so a consumer can walk it the same way; the key set legitimately differs
    /// because the underlying build systems differ.
    fn compiler_verbose_config() -> Value {
        let mut sections: HashMap<String, Value> = HashMap::new();

        let mut raku_section: HashMap<String, Value> = HashMap::new();
        raku_section.insert(
            "implementation".to_string(),
            Value::str_from(Self::COMPILER_NAME),
        );
        raku_section.insert(
            "version".to_string(),
            Value::str_from(env!("CARGO_PKG_VERSION")),
        );
        raku_section.insert(
            "language-revision".to_string(),
            Value::str(crate::parser::current_language_version()),
        );
        raku_section.insert("auth".to_string(), Value::str_from(Self::COMPILER_AUTH));
        sections.insert(
            "Raku".to_string(),
            Value::hash_with_data(Value::hash_arc(raku_section)),
        );

        let mut mutsu_section: HashMap<String, Value> = HashMap::new();
        mutsu_section.insert(
            "version".to_string(),
            Value::str_from(env!("CARGO_PKG_VERSION")),
        );
        mutsu_section.insert("id".to_string(), Value::str(Self::compiler_id()));
        // The target triple this binary was compiled for. Truthful build data
        // that is available without a build database.
        mutsu_section.insert(
            "target-os".to_string(),
            Value::str_from(std::env::consts::OS),
        );
        mutsu_section.insert(
            "target-arch".to_string(),
            Value::str_from(std::env::consts::ARCH),
        );
        mutsu_section.insert(
            "target-family".to_string(),
            Value::str_from(std::env::consts::FAMILY),
        );
        sections.insert(
            "mutsu".to_string(),
            Value::hash_with_data(Value::hash_arc(mutsu_section)),
        );

        // The kernel/distro sections mirror rakudo's, and are built from the
        // very same instances that answer `$*KERNEL` / `$*DISTRO`, so the two
        // views can never disagree.
        sections.insert(
            "kernel".to_string(),
            Self::instance_attrs_as_config(&Self::make_kernel_instance(), KERNEL_CONFIG_KEYS),
        );
        sections.insert(
            "distro".to_string(),
            Self::instance_attrs_as_config(&Self::make_distro_instance(), DISTRO_CONFIG_KEYS),
        );

        Value::hash_with_data(Value::hash_arc(sections))
    }

    /// Project the named attributes of a native info object into a flat
    /// string-valued config map, skipping any the instance does not carry.
    fn instance_attrs_as_config(instance: &Value, keys: &[&str]) -> Value {
        let mut out: HashMap<String, Value> = HashMap::new();
        if let ValueView::Instance { attributes, .. } = instance.view() {
            for key in keys {
                if let Some(val) = attributes.as_map().get(*key) {
                    out.insert((*key).to_string(), Value::str(val.to_string_value()));
                }
            }
        }
        Value::hash_with_data(Value::hash_arc(out))
    }
}

const KERNEL_CONFIG_KEYS: &[&str] = &[
    "name", "version", "release", "hardware", "arch", "bits", "hostname",
];

const DISTRO_CONFIG_KEYS: &[&str] = &[
    "name", "version", "release", "auth", "desc", "is-win", "path-sep",
];
