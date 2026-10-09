use v6;
use Test;

# Found via the SBOM::CycloneDX distribution: `subset bom-ref` checked against
# `Cool` answered False inside a routine that has a `$bom-ref` lexical, because
# the scalar (holding the type object `Any` when undefined) was taken for an
# alias of the subset.

plan 6;

subset bom-ref of Str where *.chars > 0;
my $t := bom-ref;

sub same-name(:$bom-ref) { ($t ~~ Cool, $t ~~ Str, bom-ref ~~ Str) }
sub other-name(:$bomref) { ($t ~~ Cool, $t ~~ Str, bom-ref ~~ Str) }

is-deeply same-name(), (True, True, True), 'undefined same-named scalar does not shadow the subset';
is-deeply same-name(:bom-ref<q>), (True, True, True), 'defined same-named scalar does not shadow the subset';
is-deeply other-name(), (True, True, True), 'control: differently named scalar';
ok bom-ref ~~ Cool, 'bare subset type object does Cool';
nok $t ~~ Int, 'subset of Str is not an Int';

sub mixed(:$bom-ref) { $t ~~ Int }
nok mixed(), 'a same-named scalar does not make the subset match an unrelated type';
