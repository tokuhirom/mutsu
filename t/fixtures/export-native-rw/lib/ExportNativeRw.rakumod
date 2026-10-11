use NativeCall;
sub frexp(num64, int32 is rw) returns num64 is native("m", v6) { * }
sub pl(uint32 $x is rw) { $x = 5 }
my %all = MY::.grep({ .key eq '&frexp'|'&pl' });
sub EXPORT { %all }
