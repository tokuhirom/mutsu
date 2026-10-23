my constant HL = %( ascii => '-', double => '=' );
class WCls {
  multi method mm($x, :$style where HL = 'double') { "mm $style" }
}
role WRole {
  multi method rmm($x, :$style where HL = 'double') { "rmm $style" }
}
sub s($x, :$style where HL = 'double') is export { "sub $style" }
multi sub ms($x, :$style where HL = 'double') is export { "msub $style" }
