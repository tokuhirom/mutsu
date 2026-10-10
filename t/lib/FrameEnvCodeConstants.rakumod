unit module FrameEnvCodeConstants;

sub alternatives(*@a) { @a.join('|') }

constant &alt is export(:shortcuts, :ALL) = &alternatives;
constant &tagged is export(:ALL) = -> $x { "tag:$x" };

our sub own-alt(*@a) { alt(|@a) }
our sub make-closure() { -> $y { alt($y, 'z') } }
our sub own-qualified() { &FrameEnvCodeConstants::tagged('q') }
