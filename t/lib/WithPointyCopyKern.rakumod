class WithPointyCopyKern {
    has %.metrics;
    method KernData { $.metrics<KernData> }
    method kern { my $k = self.KernData; for $k<R> { with .<V> -> $kk is copy { } }; 1 }
}
class WithPointyCopyKern::Sub is WithPointyCopyKern {
    constant Data = ${:KernData(${:R(${:V(-80)}), :V(${:A(-1)})})};
    method metrics { Data }
}
