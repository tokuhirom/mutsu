my class errno { method symbol { "ENOENT" } }
my $proxy := Proxy.new(FETCH => -> $ { UNIT::errno }, STORE => -> $, $v { });
my sub errno() is export is raw { $proxy }
