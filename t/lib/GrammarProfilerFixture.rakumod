use v6;

# Fixture for t/exporthow-grammar-how.t — a custom grammar metaclass in the
# style of roast's Advent::GrammarProfiler (integration/advent2011-day07.t):
# EXPORTHOW installs a Metamodel::GrammarHOW subclass whose find_method wraps
# token dispatch in a profiling closure.

our %timing;
our %cursor-names;
our %obj-names;
our $matched;

my class ProfiledGrammarHOW is Metamodel::GrammarHOW {

    method find_method($obj, $name) {
        my $meth := callsame;
        substr($name, 0, 1) eq '!' || $name eq any(<parse CREATE Bool defined MATCH raku perl name BUILD TWEAK DESTROY new bless BUILDALL sink>) ??
            $meth !!
            -> $c, |args {
                %cursor-names{$c.^name}++;
                %obj-names{$obj.^name}++;
                my $grammar = $obj.WHAT.raku;
                %timing{$grammar} //= {};
                %timing{$grammar}{$meth.name} //= {};
                my %t := %timing{$grammar}{$meth.name};
                my $start = now;
                my $result := $meth($c, |args);
                %t<time> += now - $start;
                %t<calls>++;
                $matched++ if $result.MATCH;
                $result
            }
    }

    method publish_method_cache($obj) {
        # no caching, so we always hit find_method
    }

}

sub get-timing is export { %timing }
sub reset-timing is export { %timing = (); %cursor-names = (); %obj-names = (); $matched = 0 }
sub get-obj-names is export { %obj-names }
sub get-matched is export { $matched }
sub get-cursor-names is export { %cursor-names }

my module EXPORTHOW { }
EXPORTHOW.WHO.<grammar> = ProfiledGrammarHOW;
