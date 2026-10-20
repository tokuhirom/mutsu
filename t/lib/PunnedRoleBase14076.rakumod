unit role PunnedRoleBase14076;
has Int $.obj-num is rw;
method required(Str:D $pkg) is raw { do require ::($pkg) }
