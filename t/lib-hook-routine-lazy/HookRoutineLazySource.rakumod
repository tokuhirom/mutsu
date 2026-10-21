my sub hook-fn2($x) { $x + 100 }
my sub EXPORT(*@names) { Map.new( @names.map: { "&$_" => &hook-fn2 } ) }
