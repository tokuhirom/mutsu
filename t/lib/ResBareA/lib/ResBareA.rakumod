# Deliberately has NO `unit module ResBareA;` declaration: its top-level subs
# compile under the generic "GLOBAL" package, the same shape Cro::HTTP::Router
# and its callers use (see t/nested-closure-resources-file-attribution.t).
sub bare-a-greeting-closure() is export {
    -> { %?RESOURCES<greeting.txt>.slurp(:close).trim }
}
