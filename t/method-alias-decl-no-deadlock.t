use Test;

plan 2;

# ADR-0019 F4c-9b regression: `our &alias ::= &method` inside a class body
# used to deadlock (same-thread recursive RwLock acquisition) because
# `class_body_code_alias` held the `self.registry()` read guard alive
# through an `if let Some(overloads) = self.registry().user_method_overloads(...)`
# block that then called `self.registry_mut()` inside it. Mirrors
# roast/S13-syntax/aliasing.t's own repro.

my class Baz {
    method bar() { 42 }
    our &baz ::= &bar;
}

my $ret;
lives-ok {
    my $obj = Baz.new;
    $ret    = $obj.baz();
}, 'calling an aliased method does not deadlock';
is $ret, 42, 'the aliased method returned the right thing';
