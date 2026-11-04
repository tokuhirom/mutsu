use Test;

# Entity codes in a Pod paragraph (#14880): `E<gt>`, `E<0x41>`, `E<amp;lt>`
# are `Doc::Markup` nodes of letter `E` with no atoms and a `meta` of one
# pair per `;`-separated item (the character as its value). A name is
# looked up in the HTML5 table (first code point), a number is a code point,
# anything else keeps its own spelling. Shapes measured on rakudo 2026.09.

plan 12;

use experimental :rakuast;

sub entity(Str $content) {
    my $src = "=begin pod\nA E<$content> B\n=end pod\n";
    my $m = $src.AST.statements[0].paragraphs[0].atoms.grep(RakuAST::Doc::Markup)[0];
    "atoms={$m.atoms.raku} meta={$m.meta.raku}"
}

is entity('gt'), 'atoms=() meta=(:gt(">"),)', 'a named entity';
is entity('amp'), 'atoms=() meta=(:amp("\&"),)', 'amp';
is entity('Aacute'), 'atoms=() meta=(:Aacute("Á"),)', 'a non-ASCII entity';
is entity('NotEqualTilde'), 'atoms=() meta=(:NotEqualTilde("≂"),)', 'only the first code point of a multi-code-point entity';
is entity('0x41'), 'atoms=() meta=("0x41" => "A",)', 'a hexadecimal number';
is entity('123'), 'atoms=() meta=("123" => "\{",)', 'a decimal number';
is entity('0b1000001'), 'atoms=() meta=("0b1000001" => "A",)', 'a binary number';
is entity('gt;lt'), 'atoms=() meta=(:gt(">"), :lt("<"))', 'a list of entities';
is entity('65;nbsp;nope'), "atoms=() meta=(\"65\" => \"A\", :nbsp(\"\x[A0]\"), :nope(\"E<nope>\"))", 'numbers, names and an unknown name';
is entity('foo|gt;lt'), 'atoms=("foo",) meta=(:gt(">"), :lt("<"))', 'text, then entities after a bar';
is entity(''), 'atoms=() meta=()', 'an empty code';
is entity('0xZZ'), 'atoms=() meta=("0xZZ" => "E<0xZZ>",)', 'a malformed number keeps its spelling';
