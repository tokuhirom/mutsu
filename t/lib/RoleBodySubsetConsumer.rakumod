use v6;

# The `PDF::COS::Tie` shape: a role whose body imports a type from another
# module and names it in a method signature. The signature is validated when
# a class composes this role, in a scope where `IndRef` is not lexically
# visible, so the import has to have registered the type globally by then.
role RoleBodySubsetConsumer {
    use RoleBodySubsetProvider :IndRef;

    method deref(IndRef $ind-ref) { $ind-ref.value }
}
