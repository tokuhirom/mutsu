use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    // --- VM ---

    /// `$path.absolute` for a path that need not exist: prepend `$*CWD` when it
    /// is relative and fold away `.` / `..` components lexically (no `realpath`,
    /// so nothing is resolved through symlinks and a missing file is fine).
    fn absolute_lexical_path(path: &std::path::Path) -> String {
        use std::path::Component;
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            match std::env::current_dir() {
                Ok(cwd) => cwd.join(path),
                Err(_) => path.to_path_buf(),
            }
        };
        let mut out = std::path::PathBuf::new();
        for comp in absolute.components() {
            match comp {
                Component::CurDir => {}
                Component::ParentDir => {
                    out.pop();
                }
                other => out.push(other),
            }
        }
        out.to_string_lossy().to_string()
    }

    /// `VM.platform-library-name($name, :$version)`: the OS-specific file name
    /// of the shared library `$name` (`libfoo.so`, `libfoo.dylib`, `foo.dll`),
    /// as an `IO::Path`.
    // Cost: O(n), n = chars of the name.
    pub(crate) fn platform_library_name(&self, name: &Value, named: &[Value]) -> Value {
        let name = name.to_string_value();
        // `:version` names an ABI-versioned library. A distribution ships
        // the versioned file (`libpq.so.5`) and only the `-dev` package
        // installs the bare `libpq.so` symlink, so a binding that probes
        // versions — `NativeLibs::Searcher.at-runtime('pq', 'PQstatus', 5)`,
        // which DBIish's Pg driver uses — finds nothing at all when the
        // version is dropped. Rakudo appends it after the extension on
        // Linux and before it on macOS; Windows has no ABI suffix.
        let version = named
            .iter()
            .find_map(|v| match v.view() {
                ValueView::Pair(key, value) if key.as_str() == "version" => Some(value),
                _ => None,
            })
            .filter(|v| crate::runtime::types::value_is_defined(v))
            .map(|v| v.to_string_value())
            // `Version.new(5).Str` is `5`, but a `Version` gists as `v5`.
            .map(|s| s.strip_prefix('v').unwrap_or(&s).to_string());
        // Rakudo decorates only the BASENAME and puts the directory back
        // afterwards: `'/bar/foo'.IO` becomes `/bar/libfoo.so`, not
        // `lib/bar/foo.so`. A name that carries any directory at all is
        // additionally made absolute (`'./foo'` -> `$*CWD/libfoo.so`),
        // which is what makes `NativeLibs`' "load the .so I just built
        // next to me" idiom work.
        let path = std::path::Path::new(&name);
        let basename = path
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| name.clone());
        let has_dir = basename != name;
        let platform_name = match (cfg!(target_os = "macos"), cfg!(windows), version) {
            (true, _, Some(v)) => format!("lib{basename}.{v}.dylib"),
            (true, _, None) => format!("lib{basename}.dylib"),
            (_, true, _) => format!("{basename}.dll"),
            (_, _, Some(v)) => format!("lib{basename}.so.{v}"),
            (_, _, None) => format!("lib{basename}.so"),
        };
        let full = if has_dir {
            let dir = path.parent().unwrap_or(std::path::Path::new("."));
            Self::absolute_lexical_path(&dir.join(&platform_name))
        } else {
            platform_name
        };
        self.make_io_path_instance(&full)
    }

    // --- Perl ---

    /// A stable identifier for this compiler build. Rakudo's `Compiler.id`
    /// returns the git SHA of the build; mutsu has no per-build SHA, so use the
    /// crate version, which is stable across a build and distinct across
    /// releases — enough for consumers that key precomp directories on it
    /// (e.g. Repository::Precomp::Cleanup).
    /// `$*VM.request-garbage-collection` and `nqp::force_gc`: run a collect
    /// now, then the DESTROY submethods it made due.
    // Cost: one cycle collect (see `gc::collect_on_request`) plus the pending
    // DESTROYs.
    pub(crate) fn request_garbage_collection(&mut self) -> Result<(), RuntimeError> {
        crate::gc::collect_on_request();
        // Process pending DESTROY submethods for objects whose refcount
        // dropped to 0 (possibly including items the collect just freed).
        self.run_pending_instance_destroys()
    }

    pub(crate) fn compiler_id() -> String {
        format!("{}-{}", Self::COMPILER_NAME, env!("CARGO_PKG_VERSION"))
    }

    /// The compiler's own name and authority, shared by `$*RAKU.compiler`'s
    /// attributes, `.id`, and `.verbose-config` so they cannot drift apart.
    pub(in crate::runtime) const COMPILER_NAME: &'static str = "mutsu";
    pub(in crate::runtime) const COMPILER_AUTH: &'static str = "github.com/tokuhirom";

    /// The Rakudo release whose language level mutsu targets, reported as
    /// `$*RAKU.compiler.version` / `$*PERL.compiler.version`.
    ///
    /// That field is not "which build of this interpreter am I" — the ecosystem
    /// uses it as a *language-level coordinate expressed in Rakudo release
    /// dates*, and the only comparisons ever made against it are with Rakudo
    /// releases:
    ///
    /// ```raku
    /// use RakudoPrereq v2017.06;                              # RakudoPrereq
    /// use Foo::Pre201812:if($*PERL.compiler.version < v2018.12);  # Net::BGP
    /// ```
    ///
    /// Answering those with mutsu's own crate version (`v0.23.0`) is not a
    /// neutral non-answer: as a Version it sorts below every Rakudo release
    /// ever made, so every such gate resolves to its worst branch — modules
    /// refuse to load at all (`Proc::Q`, see ADR-0104), and conditional shims
    /// select their pre-2018 code path even though mutsu implements the modern
    /// semantics. Naming the release mutsu is actually measured against is both
    /// more accurate and fails in the useful direction: a feature mutsu has not
    /// implemented yet surfaces as a concrete "not implemented" error the
    /// ecosystem board can act on, instead of a spurious version block.
    ///
    /// mutsu's *own* release stays truthful and reachable, and mutsu never
    /// claims to be Rakudo: `.name` is `mutsu` (so `RakudoPrereq`'s
    /// `rakudo-only` and every `compiler.name ne 'rakudo'` check still
    /// discriminate correctly), `.release` is the crate version, `.id` is
    /// `mutsu-<crate version>`, and `.verbose-config`'s `mutsu` section carries
    /// the build facts.
    ///
    /// Bump this together with the Rakudo version the ecosystem sweep and the
    /// roast baseline are run against (`ecosystem/dists/**` `measured.raku_version`,
    /// `TODO_roast/raku-baseline.md`) — it is a claim about compatibility, so it
    /// must be backed by a measurement, not raised speculatively.
    pub(in crate::runtime) const RAKUDO_COMPAT_VERSION: &'static str = "2026.09";

    /// The `Compiler` object `$*RAKU.compiler` is, and `Compiler.new` copies.
    // Cost: O(1).
    pub(crate) fn compiler_instance() -> Value {
    let mut compiler_attrs = HashMap::new();
    compiler_attrs.insert("name".to_string(), Value::str_from(Self::COMPILER_NAME));
    compiler_attrs.insert("auth".to_string(), Value::str_from(Self::COMPILER_AUTH));
    // A Rakudo release date, not the crate version: see
    // [`Interpreter::RAKUDO_COMPAT_VERSION`]. The build's own
    // identity lives in `.release` / `.id` just below.
    compiler_attrs.insert(
        "version".to_string(),
        Value::version_from_str(Self::RAKUDO_COMPAT_VERSION),
    );
    compiler_attrs.insert(
        "signature".to_string(),
        // The `Blob` type object, as in Rakudo.
        Value::package(Symbol::intern("Blob")),
    );
    compiler_attrs.insert(
        "desc".to_string(),
        Value::str_from("mutsu Raku interpreter"),
    );
    // mutsu's own release. This is where `.version` used to answer
    // "which build is this", and it still does.
    compiler_attrs.insert(
        "release".to_string(),
        Value::str_from(env!("CARGO_PKG_VERSION")),
    );
    compiler_attrs.insert("codename".to_string(), Value::str_from("mutsu"));
    compiler_attrs.insert("id".to_string(), Value::str(Self::compiler_id()));
    Value::make_instance(Symbol::intern("Compiler"), compiler_attrs)
    }
}
