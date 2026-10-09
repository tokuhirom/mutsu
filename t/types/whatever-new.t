use Test;

plan 3;

my $w = Whatever.new;
isa-ok $w, Whatever, 'Whatever.new constructs a Whatever';
is Whatever.new.WHAT.gist, '(Whatever)', '.WHAT is (Whatever)';
dies-ok { HyperWhatever.new }, 'HyperWhatever.new still dies';
