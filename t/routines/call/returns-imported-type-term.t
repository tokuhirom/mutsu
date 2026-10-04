use Test;
use NativeCall;

# A return type spelled with an imported type's short name (`size_t` from
# NativeCall) answers that very type object, as the bare term does -- not a
# same-spelled type the core knows by that name. Upstream NativeCall reads
# `.returns.REPR` to marshal a native routine's result, so it must be the
# type the module declared.

plan 4;

sub f(--> size_t) { }
ok &f.returns === size_t, '.returns is the imported type object';
ok &f.signature.returns === size_t, '.signature.returns is too';

sub g(--> Int) { }
ok &g.returns === Int, 'a core return type is unchanged';

my subset Small of Int where * < 10;
sub h(--> Small) { }
is &h.returns.^name, 'Small', 'a lexical subset return type is unchanged';
