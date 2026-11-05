use Test;

plan 9;

# `$p = ()` is an immutable empty List shared by calls; `[]` and `{}` defaults
# stay a fresh mutable container per call (#14854).
sub list-default($a, $p = ()) { $p }
is list-default(1).^name, 'List', '() default is a List';
is list-default(1).elems, 0, '() default is empty';
dies-ok { list-default(1).push(1) }, '() default cannot be pushed to';
is list-default(1, (1, 2)).elems, 2, 'a supplied argument overrides the default';
is list-default(1).elems, 0, 'the shared default is still empty after other calls';

sub array-default($a, $p = []) { $p.push($a); $p.elems }
is array-default(1), 1, '[] default starts empty';
is array-default(2), 1, '[] default is a fresh container per call';

sub hash-default($a, $p = {}) { $p{$a} = 1; $p.elems }
is hash-default(1), 1, '{} default starts empty';
is hash-default(2), 1, '{} default is a fresh container per call';
