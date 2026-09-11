# An outer hook that depends on its `use` arguments, so the nested load must
# not just avoid the collision but hand the right hook the right arguments.
use ChainExportInner;

sub EXPORT(*@names) {
    Map.new: @names.map(-> $name { "&$name" => sub () { "$name:" ~ chain-inner() } })
}
