# A module whose exported and unexported `proto` families, and whose own
# lexical class, are reached on every call of a hot loop (#15786).
unit module ProtoMemoScope;

my class Missing { }

proto sub pms-find(|) is export {*}
multi sub pms-find(@a, $k) {
    for @a.kv -> $i, $v { return $i if $v eqv $k }
    Missing.new
}
multi sub pms-find(%h, $k) { %h{$k}:exists ?? $k !! Missing.new }

proto sub pms-kind(|) is export {*}
multi sub pms-kind(Missing $m) { 'missing' }
multi sub pms-kind($other) { 'value' }

sub pms-found($r) is export { !($r ~~ Missing) }

proto sub pms-hidden(|) {*}
multi sub pms-hidden(Int) { 'h-int' }
multi sub pms-hidden(Str) { 'h-str' }

sub pms-call-hidden($x) is export { pms-hidden($x) }
