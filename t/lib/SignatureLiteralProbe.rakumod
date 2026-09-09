unit module SignatureLiteralProbe;

# The module's own body holds both sides of the comparison, so BOTH the
# candidate signatures and the `Signature` literal come from the cached AST on
# a warm run.
multi sub probe-multi(Routine $r, :$native!) is export { 'native' }
multi sub probe-multi(Routine $r, :$symbol!) is export { 'symbol' }

our sub signature-probe is export {
    &probe-multi.candidates.map({ (.signature ~~ :(Routine, :$native!)).Str }).join(',')
}
