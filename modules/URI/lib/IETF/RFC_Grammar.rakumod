unit class IETF::RFC_Grammar;

# Thanks in part to Aaron Sherman
# and his article http://essays.ajs.com/2010/05/writing-perl-6-uri-module.html
# for inspiring this.

# below should be constant when implemented ...
my %rfc_grammar_build = (
     'rfc3986' => 'IETF::RFC_Grammar::URI'
);
my $rfc_grammar_lock = Lock.new;
my %rfc_grammar;

# Hack to give hint to ufo/Panda to build in the right order.
# remove some day when module builders are upgraded
use IETF::RFC_Grammar::URI;

has $.rfc;
has $.grammar;
has $.parse-result;

method parse($parse_str, :$rule = 'TOP') {
    $!parse-result = $!grammar.parse($parse_str, :$rule);
}

method subparse($parse_str, :$rule = 'TOP') {
    $!parse-result = $!grammar.subparse($parse_str, :$rule);
}

submethod BUILD(:$!rfc, :$!grammar) {}

method new(Str $rfc, $grammar?) {
    my $init_grammar = $grammar;

    $rfc_grammar_lock.protect: {
        if (
            (! $init_grammar.can('parse'))  and
            %rfc_grammar_build{$rfc}:exists;
        ) {
            unless %rfc_grammar{$rfc}:exists {
                my $module = %rfc_grammar_build{$rfc};
                # less disruptive fix to RT126390
                unless ($rfc eq 'rfc3986') { require ::($module); }
                %rfc_grammar{$rfc} = ::($module);
            }
            $init_grammar = %rfc_grammar{$rfc};
        }
    }
    if (! $init_grammar.can('parse')) {
        die "Need either rfc with known grammar or grammar";
    }

   return self.bless(:$rfc, :grammar($init_grammar));
}

method parse_result { $!parse-result }

# vim: expandtab shiftwidth=4
