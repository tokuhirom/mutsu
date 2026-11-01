use Test;
plan 5;

# `.VAR` on a Proxy answers the decontainerized Proxy itself (the pseudo-method
# lane of ADR-15829). Rakudo compiles `.VAR`, so a `method VAR` declared in a
# Proxy subclass is never called.
my $store = 5;
my $p := Proxy.new(FETCH => method () { $store }, STORE => method ($v) { $store = $v });
is $p.VAR.^name, 'Proxy', '.VAR on a Proxy is the Proxy';
my $q := Proxy.new(FETCH => method () { 1 }, STORE => method ($v) { });
is $q.VAR.WHAT.gist, '(Proxy)', '.WHAT of the .VAR is Proxy';

class Sub is Proxy { method VAR { "mine" } }
my $s := Sub.new(FETCH => method () { $store }, STORE => method ($x) { $store = $x });
is $s.VAR.^name, 'Sub', '.VAR on a Proxy subclass is the subclass instance';
isnt $s.VAR, 'mine', "a Proxy subclass's own VAR is not called";

my $n = 0;
for ^3 { $n++ if $p.VAR.^name eq 'Proxy' }
is $n, 3, 'repeated .VAR keeps answering a Proxy';
