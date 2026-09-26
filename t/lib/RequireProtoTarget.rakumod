unit module RequireProtoTarget;

our proto sub shared-name(|) is export {*}
our multi sub shared-name(Int $value) is export { $value }
