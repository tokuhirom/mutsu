# Fixture for t/oo/class/stub-class-definition-after-versioned-decl.t
# (CSS::Grammar's shape): a stubbed child, then its versioned parent, then the
# child's real definition naming that parent.
grammar StubThenVersionedParent::Core {...}

grammar StubThenVersionedParent:ver<0.5.0>:auth<zef:test> {
    token word { \w+ }
}

grammar StubThenVersionedParent::Core is StubThenVersionedParent {
    token TOP { <word> }
}

class StubThenVersionedClass::Kid {...}
class StubThenVersionedClass:ver<1.2> { method hi { 'parent' } }
class StubThenVersionedClass::Kid is StubThenVersionedClass { }
