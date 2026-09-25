use Test;

plan 2;

# A nested with over a loop element must write back through the enclosing hash
# element rather than treating the child value as an independent topic.
my &from-json = { Rakudo::Internals::JSON.from-json: $^a };
my %responses = from-json('{"redirects":[{"content":"\\n"}]}');
for %responses<redirects>.grep(*.defined) {
    with .<content> {
        $_ = Buf[uint8].new: .encode;
    }
}
is %responses<redirects>[0]<content>.^name, 'Buf[uint8]',
    'nested with writes a converted value back through the loop element';
is-deeply %responses<redirects>[0]<content>, Buf[uint8].new(10),
    'nested with preserves the converted buffer in the enclosing hash';
