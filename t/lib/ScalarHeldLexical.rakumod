unit module ScalarHeldLexical;

# A compunit's own file-scope SCALAR-held containers, in every declarator that
# can hold one. The sigil is `$`, so these take the scalar unit-lexical lane
# (ADR-0024) rather than the `@`/`%` container lane (ADR-0039) --
# `ContainerSlotLexical.rakumod` covers that one. Each routine lets the consumer
# prove the module's own binding is the one its routines mutate, while the
# consumer's same-named `my $...` stays untouched.

my $arr = [<a b c>];
my $hsh = { k => 'v' };
our $ours = [<a b c>];
state $st = [<a b c>];

sub sh-push($v) is export { $arr.push($v) }
sub sh-append($v) is export { $arr.append($v) }
sub sh-unshift($v) is export { $arr.unshift($v) }
sub sh-prepend($v) is export { $arr.prepend($v) }
sub sh-pop() is export { $arr.pop }
sub sh-shift() is export { $arr.shift }
sub sh-splice() is export { $arr.splice(0, 1) }
sub sh-elemset($v) is export { $arr[0] = $v }
sub sh-peek() is export { $arr.join(",") }

sub sh-hset($k) is export { $hsh{$k} = 1 }
sub sh-hpush($k) is export { $hsh.push($k => 1) }
sub sh-hpeek() is export { $hsh.keys.sort.join(",") }

sub sh-our-push($v) is export { $ours.push($v) }
sub sh-our-peek() is export { $ours.join(",") }

sub sh-state-push($v) is export { $st.push($v) }
sub sh-state-peek() is export { $st.join(",") }

# A class declared in the module: its methods carry the module mainline as
# `captured_env`, which is a different resolution path from a plain `sub`.
my @carr = <a b c>;
my $cscalar = "module";

class ScalarHeldHolder is export {
    method mpush($v) { $arr.push($v) }
    method mpeek() { $arr.join(",") }
    method apeek() { @carr.join(",") }
    method speek() { $cscalar }
}
