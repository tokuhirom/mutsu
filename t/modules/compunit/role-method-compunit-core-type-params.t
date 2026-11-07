use Test;

plan 9;

# A role method signature may name any of the `CompUnit::` core types; the
# role body is validated when the role is declared, so an unknown typename
# would be rejected before the first call.
for <CompUnit::Handle CompUnit::Loader CompUnit::PrecompilationRepository
     CompUnit::PrecompilationRepository::None CompUnit::PrecompilationUnit
     CompUnit::PrecompilationUnit::File CompUnit::Repository::AbsolutePath
     CompUnit::Repository::Unknown> -> $type {
    lives-ok { EVAL "role R \{ method m($type \$x) \{ ... \} \}" }, "role method parameter typed $type";
}

# The declaration shape from Language/compilation.rakudoc.
class CompUnit::Store {};
lives-ok {
    EVAL q:to/CODE/;
    role CompUnit::Repository {
        has CompUnit::Repository $.next-repo is rw;
        method need(CompUnit::DependencySpecification $spec,
                    CompUnit::PrecompilationRepository $precomp,
                    CompUnit::Store :@precomp-stores
                    --> CompUnit:D
                    )
            { ... }
        method loaded( --> Iterable ) { ... }
        method id( --> Str ) { ... }
    }
    CODE
}, 'the CompUnit::Repository role from the compilation chapter declares';
