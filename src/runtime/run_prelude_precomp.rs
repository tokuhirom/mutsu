use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// The `CompUnit::Precompilation*` classes (#11541), written in Raku.
///
/// mutsu loads modules from source, so precompilation is only a parse cache
/// and no store ever holds a compiled unit. These classes give the *API*
/// real behaviour all the same: `try-load` compiles and runs the dependency's
/// source as its own compunit (an `EVAL`) and hands back a
/// `CompUnit::Handle` whose `.unit` exposes the unit's pad entries
/// (`$=pod`, `$?PACKAGE`). That is what Pod::Load relies on to read the Pod of
/// a file. Nothing is written to the store's prefix.
pub(super) const PRECOMP_API_PRELUDE: &str = r#"
class GLOBAL::CompUnit::PrecompilationId {
    has Str $.id;
    multi method new(Str:D $id) { self.bless(:$id) }
    method new-from-string(Str:D $str) { self.bless(:id(nqp::sha1($str))) }
    method Str { $!id }
    method gist { $!id }
    method IO { $!id.substr(0, 2).IO.add($!id) }
}

role GLOBAL::CompUnit::PrecompilationDependency {
}

class GLOBAL::CompUnit::PrecompilationDependency::File does CompUnit::PrecompilationDependency {
    has CompUnit::PrecompilationId $.id;
    has Str $.src;
    has Str $.checksum is rw;
    has $.spec;
    method Str { $!src }
    method serialize(--> Str:D) {
        join "\0", $!id.Str, $!src, $!checksum // '', $!spec.short-name
    }
}

role GLOBAL::CompUnit::PrecompilationStore {
}

class GLOBAL::CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    has IO::Path $.prefix;
    method new(:$prefix) { self.bless(:prefix($prefix.defined ?? $prefix.IO !! Nil)) }
    method path(Str $compiler-id, CompUnit::PrecompilationId $precomp-id, :$extension = '') {
        my $dir = $!prefix.add($compiler-id).add($precomp-id.Str.substr(0, 2));
        $dir.add($precomp-id.Str ~ $extension)
    }
    method unit-path(Str $compiler-id, CompUnit::PrecompilationId $precomp-id) {
        self.path($compiler-id, $precomp-id)
    }
    method load-unit(Str $compiler-id, CompUnit::PrecompilationId $precomp-id) { Nil }
    method load-repo-id(Str $compiler-id, CompUnit::PrecompilationId $precomp-id) { Nil }
    method remove-from-cache(CompUnit::PrecompilationId $precomp-id) { Nil }
}

class GLOBAL::CompUnit::PrecompilationStore::FileSystem is CompUnit::PrecompilationStore::File {
}

class GLOBAL::CompUnit::PrecompilationRepository::Default {
    has $.store;
    method load(CompUnit::PrecompilationId $id, |) { Nil }
    method may-precomp { True }
    method try-load(
        $dependency,
        IO::Path :$source,
        :@precomp-stores,
        :$repo-id,
        :$resolve = True,
    ) {
        my $path = ($dependency.defined && $dependency.can('src') ?? $dependency.src !! Nil)
            // $source;
        fail "Cannot find the source of the unit to load" unless $path.defined && $path.IO.e;
        my $code = $path.IO.slurp;
        my @pod = EVAL($code ~ "\n\$=pod");
        my %unit;
        %unit<$=pod> := @pod;
        %unit<$?PACKAGE> = GLOBAL;
        CompUnit::Handle.new(:unit(%unit))
    }
}
"#;

impl Interpreter {
    /// Prepend [`PRECOMP_API_PRELUDE`] to a program (or module) that names
    /// the precompilation API. Skipped when it declares its own.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_precomp_api_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("CompUnit::Precompilation")
            || source.contains("class CompUnit::Precompilation")
        {
            return;
        }
        use std::sync::OnceLock;
        static PRECOMP_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = PRECOMP_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(PRECOMP_API_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
