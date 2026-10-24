unit class NestedExportInMethod;
has $.v;
has $.bar is rw;
submethod BUILD ( :$!v, :$!bar ) {
  my $twice = $!v * 2;
  multi sub nem-log ( 'a', *%o ) is export { "a:{$!v}:{$twice}" }
  multi sub nem-log ( 'b', *%o ) is export { "b:{self.v}:{$!bar}" }
}
