use ProtoImpRoutines;
unit class ProtoImpClass;
proto method run ( | ) {
  my $before = pim-plain();
  my $r = {*};
  "$before/$r/{pim-multi('a')}";
}
multi method run ( ) { 'm:' ~ pim-plain() }
proto method guarded ( | ) {
  {*}
  CATCH { default { return "caught:" ~ pim-plain() } }
}
multi method guarded ( ) { die 'boom' }
