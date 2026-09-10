use Issue7861::Parent;
unit module Issue7861::Unit;

role ExternalChild does Issue7861::Parent::R {
    method external-child { 'external-child' }
}

role ExternalGrandchild does ExternalChild {
    method external-grandchild { 'external-grandchild' }
}

module Nested {
    role R {
        method nested-parent { 'nested-parent' }
    }
}

role NestedChild does Nested::R {
    method nested-child { 'nested-child' }
}

role NestedGrandchild does NestedChild {
    method nested-grandchild { 'nested-grandchild' }
}
