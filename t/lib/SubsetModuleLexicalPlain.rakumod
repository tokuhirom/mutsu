my constant $hdr = "H";
my subset Hyp of Str where .starts-with($hdr);
sub chk(Str $x) is export { $x ~~ Hyp }
