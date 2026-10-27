class OpaqueVecMul {
    has @.c;
    method dim { @.c.elems }
    multi method Num() { die "no Num" }
}
multi infix:<×>(OpaqueVecMul $a where { $a.dim == 3 }, OpaqueVecMul $b where { $b.dim == 3 }) is export { "cross" }

