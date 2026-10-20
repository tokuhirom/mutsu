use Test;

# `.name` on an instance of a built-in class that declares no `name` is
# `X::Method::NotFound`, as in Rakudo (#13630); the classes that declare one
# answer it from their row.

plan 8;

throws-like { Date.today.name }, X::Method::NotFound, 'Date';
throws-like { IO::Path.new('x').name }, X::Method::NotFound, 'IO::Path';
throws-like { Instant.from-posix(1).name }, X::Method::NotFound, 'Instant';
throws-like { Duration.new(1).name }, X::Method::NotFound, 'Duration';

ok $*THREAD.name.defined, 'Thread.name';
is $*KERNEL.name.WHAT, Str, 'Kernel.name';
is Encoding::Registry.find('utf8').name, 'utf8', 'Encoding::Builtin.name';
class C { has $.x }
is C.^attributes.first.name, '$!x', 'Attribute.name';
