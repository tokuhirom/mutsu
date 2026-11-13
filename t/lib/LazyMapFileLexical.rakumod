# Package-less module file (no `unit` declarator), as Text::T9 is: its
# file-scope lexicals are read by lazy `.map`/`.grep` callbacks that run only
# when the importing script consumes the returned Seq.
my constant %keys = < k 5  i 4  s 7 >;
my %vals = k => 5, i => 4;

sub mapped(@w) is export { @w.map: -> $w { %keys{$w} } }
sub bare-block(@w) is export { @w.map({ %keys{$_} }) }
sub plain-hash(@w) is export { @w.map: -> $w { %vals{$w} } }
sub grepped(@w) is export { @w.grep: -> $w { %vals{$w}:exists } }
sub nested(@w) is export { @w.map: -> $w { $w.comb.map({ %keys{$_} }).join } }
sub t9(Str(Int) $input, @words) is export {
    my int $chars = $input.chars;
    @words.map: -> $word {
        $word if $word.chars == $chars && $word.comb.map({ %keys{$_} }).join eq $input
    }
}
