# Imports an operator for its OWN use, and uses it at load time.
unit module RoleBodyLoadOp::Calc;

use RoleBodyLoadOp::Units :pt;

our $size = 10pt;
my constant %Sizes = %( :small(6pt) );
our sub small { %Sizes<small> }
