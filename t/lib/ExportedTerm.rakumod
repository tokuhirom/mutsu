unit module ExportedTerm;

our $*STATE;

#| A term the importer should be able to write as a bareword, the way
#| Cro::HTTP::Router exports `term:<request>` / `term:<response>`.
sub term:<answer>() is export { 42 }

sub term:<current-state>() is export {
    $*STATE // die "current-state is only usable with \$*STATE set"
}
