use Test;

# A declaration with a tail (`my $x = 1, 123`) is sunk like any other statement:
# only the trailing items warn, never the declared head. The `.AST` parse keeps
# the tail as a `DeclTail` node, and the sink analysis has to see through it.
sub err-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :err, :out;
    $proc.err.slurp
}

like err-of('my $x = 1, 123'), /'Useless use' .* '123'/, 'the trailing item of a declaration warns';
unlike err-of('my $x = 1, 123'), /'$x'/, 'the declared head does not';
is err-of('my $x = 1 and 5'), '', 'a loose word tail over a declaration is not constant-sunk';
done-testing;
