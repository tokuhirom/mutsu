use Test;

# `pack` / `unpack` are experimental (#13346): the routine form is undeclared
# and the `Blob.unpack` method raises X::Experimental unless the file says
# `use experimental :pack`. Each program runs in its own process because the
# routine form is judged at compile time, for the whole unit.

plan 8;

sub run-program(Str $code) {
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err);
    ($proc.exitcode, $proc.out.slurp(:close).chomp, $proc.err.slurp(:close))
}

my ($code, $out, $err) = run-program('say pack("C*", 1, 2)');
isnt $code, 0, 'pack() without the pragma fails to compile';
like $err, /'Undeclared routine'/, 'it is an undeclared routine';

($code, $out, $err) = run-program('say Buf.new(1,2,3).unpack("C*")');
isnt $code, 0, '.unpack without the pragma fails';
like $err, /"Use of the 'unpack' method is experimental"/, 'with the X::Experimental message';

($code, $out, $err) = run-program(
    'try Buf.new(1,2,3).unpack("C*"); say $!.^name; say $!.use');
is $out, "X::Experimental\npack", 'the exception is an X::Experimental naming the feature';

($code, $out, $err) = run-program('use experimental :pack; say Buf.new(1,2,3).unpack("C*")');
is $out, '(1 2 3)', '.unpack works with the pragma';

($code, $out, $err) = run-program('use experimental :pack; say pack("A3", "hi").decode.trim');
is $out, 'hi', 'pack() works with the pragma';

($code, $out, $err) = run-program(
    'use experimental :pack; sub f($b) { $b.unpack("C*") }; say f(Buf.new(4,5))');
is $out, '(4 5)', 'the pragma covers routines of the same file';
