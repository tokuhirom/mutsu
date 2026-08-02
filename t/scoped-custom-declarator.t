use Test;
use lib 'modules/OO-Monitors/lib';
use OO::Monitors;

plan 6;

# A declarator registered through EXPORTHOW::DECLARE works with a `my`/`our`
# scope prefix, not just bare. Cro::HTTP::Client declares `my monitor
# ConnectionCache { ... }`, which used to be a fatal "Malformed my".
my monitor Counter {
    has $.n is rw = 0;
    method bump() { $!n = $!n + 1; $!n }
}

my $c = Counter.new;
is $c.bump, 1, 'my monitor: method call works';
is $c.bump, 2, 'my monitor: state is kept';
is Counter.HOW.^name, 'MetamodelX::MonitorHOW', 'my monitor gets the declarator HOW';

our monitor Shared {
    has $.label;
}
is Shared.new(label => 'x').label, 'x', 'our monitor works too';
is Shared.HOW.^name, 'MetamodelX::MonitorHOW', 'our monitor gets the declarator HOW';

# A `my`-scoped declarator is lexical to its block.
{
    my monitor Inner { has $.v }
    # no-op; just checking it parses and constructs
    Inner.new(v => 1);
}
nok ::("Inner").defined, 'my monitor is lexical to its block';
