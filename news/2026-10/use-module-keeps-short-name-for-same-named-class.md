# `use A::Reg` no longer publishes the module as `Reg`

#13940 publishes a nested package under its short name when a type of the same short name is declared inside it. It also fired for a compunit's own package (`unit module A::Reg; class Reg {}`), so `use A::Reg` made `Reg` the module and a later `constant Reg = A::Reg::Reg` lost to it in a `--> Reg` return type. The compunit's own package is now left out.
