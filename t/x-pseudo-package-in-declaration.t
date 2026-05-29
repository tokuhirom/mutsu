use Test;

plan 9;

# X::PseudoPackage::InDeclaration - pseudo-packages cannot be used as declaration names

throws-like { EVAL 'unit module MY;' },
  X::PseudoPackage::InDeclaration,
  'MY is not allowed as a module name';

throws-like { EVAL 'unit module OUR;' },
  X::PseudoPackage::InDeclaration,
  'OUR is not allowed as a module name';

throws-like { EVAL 'unit module OUTER;' },
  X::PseudoPackage::InDeclaration,
  'OUTER is not allowed as a module name';

throws-like { EVAL 'unit module CALLER;' },
  X::PseudoPackage::InDeclaration,
  'CALLER is not allowed as a module name';

throws-like { EVAL 'unit module DYNAMIC;' },
  X::PseudoPackage::InDeclaration,
  'DYNAMIC is not allowed as a module name';

throws-like { EVAL 'unit module PROCESS;' },
  X::PseudoPackage::InDeclaration,
  'PROCESS is not allowed as a module name';

throws-like { EVAL 'unit module COMPILING;' },
  X::PseudoPackage::InDeclaration,
  'COMPILING is not allowed as a module name';

throws-like { EVAL 'unit class MY;' },
  X::PseudoPackage::InDeclaration,
  'MY is not allowed as a class name';

throws-like { EVAL 'unit module GLOBAL;' },
  Exception,
  'GLOBAL is rejected as a module name';
