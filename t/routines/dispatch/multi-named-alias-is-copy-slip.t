use Test;
# From WWW::Ollama (via LLM::Functions): `:f(:$format) is copy` bound to a
# forwarded read-only parameter that arrives through a `|%args` slip must own a
# fresh container.
plan 4;

sub oc($input, :f(:$format) is copy = Whatever, *%args) {
    $format = 'v' if $format.isa(Whatever);
    $format
}
sub bu(:$format = Whatever, *%args) { oc('', :$format, |%args) }
is bu(), 'v', 'unsupplied default, forwarded through a slip';
is bu(:format<x>), 'x', 'supplied value';

sub oc2($input, :f(:$format) is copy = Whatever, *%args) { $format = 'w'; $format }
sub bu2(:$format = 5, *%args) { my $r = oc2('', :$format, |%args); "$r $format" }
is bu2(), 'w 5', 'the caller keeps its own value';
sub bu3(:$format = 5) { oc2('', :$format) }
is bu3(), 'w', 'without a slip';
