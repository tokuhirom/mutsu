use Test;

# A method's `is DEPRECATED(...)` message must survive both role composition
# and `augment class` — registration_role_method.rs and
# registration_class_augment.rs each independently build a MethodDef from
# Stmt::MethodDecl and previously hard-coded deprecated_message to None
# instead of threading the parsed message through.

plan 2;

{
    role R { method foo() is DEPRECATED('use bar instead') { 42 } }
    class C does R { }
    C.new.foo;
    ok Deprecation.report ~~ m/'use bar instead'/,
        'role-composed method keeps its is DEPRECATED message';
}

{
    use MONKEY-TYPING;
    class D { }
    augment class D { method quux() is DEPRECATED('use baz instead') { 42 } }
    D.new.quux;
    ok Deprecation.report ~~ m/'use baz instead'/,
        'augment class method keeps its is DEPRECATED message';
}

# vim: expandtab shiftwidth=4
