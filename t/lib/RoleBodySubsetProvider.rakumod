use v6;

# The `PDF::COS` shape: a module whose types are declared inside a ROLE body
# and handed out with `is export(:tag)`. Consumers `use` the tag long before
# any class composes the role, so the subset has to be installed (and
# exported) when the role is declared, not at composition time.
role RoleBodySubsetProvider {
    my subset IndRef of Pair is export(:IndRef) where { .key eq 'ind-ref' };

    method provider-marker() { 'provider' }
}
