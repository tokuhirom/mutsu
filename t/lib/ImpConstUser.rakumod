use ImpConstEnum;
sub kinds is export { (IK_A, IK_B, IK_C, IK_D).map(*.value).join(",") }
class ImpConstUser is export {
    has ImpKind @.rec;
    method fill { (1, 2).map({ my $o = ImpConstUser.new; $o.rec[0] = IK_B; $o.rec[0] = IK_D; $o }).eager }
}
