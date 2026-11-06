use RxOurProvider;
sub rx-plain() is export { rx-check("abc") }
sub rx-gathered() is export { gather { for <abc 123> -> $x { take rx-check($x) } } }
sub rx-assigned() is export { my @r = gather { take rx-check("xyz") }; @r }
