use RegexConstExporter;

# A bare sub and a class method: both interpolate an imported constant.
sub subst-it(Str $s is copy) is export { $s ~~ s:g[ $WORD ] = '"'; $s }
sub match-all(Str $s) is export { ($s ~~ m:g/ $WORD /).elems }
sub match-one(Str $s) is export { ?($s ~~ m/ x $WORD /) }
sub private-use(Str $s is copy) is export { $s ~~ s:g[ $SENT ] = '"'; $s }

class Holder is export {
    method subst-it(Str $s is copy) { $s ~~ s:g[ $WORD ] = '"'; $s }
}
