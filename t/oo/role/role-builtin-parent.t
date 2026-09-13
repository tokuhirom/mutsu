use v6;
use Test;

plan 1;

# A role may inherit from a builtin concrete parent. ASN::META uses this shape
# for its recursive Set-backed type roles.
role BuiltinSetParentRole[::T] is Set {
    method type { T }
}
class BuiltinSetParentConsumer does BuiltinSetParentRole[Int] { }

is BuiltinSetParentConsumer.^parents[0].^name,
    'Set',
    'builtin concrete parent is retained when the role is composed';
