use Test;
use lib 't/lib';
use ImpConstUser;
use ImpConstEnum;

plan 3;

# `$obj.attr[0] = CONST` inside a method's closure, with CONST imported from
# another module, must store a copy of the value. It used to store the
# constant's own cell, so the next store through the element (`= IK_D`)
# rewrote the constant itself (Math::NIntegrate's range-end flags).
is kinds(), '0,1,2,3', 'constants start intact';
my @objs = ImpConstUser.new.fill;
is kinds(), '0,1,2,3', 'constants are intact after element stores in a method closure';
is @objs.map(*.rec[0]).join(","), 'IK_D,IK_D', 'the elements hold the last value stored';
