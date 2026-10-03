# A package-less module whose exported plain subs a `need` must not import
# (#11080).
sub npe-exported is export { 'exported' }
sub npe-helper { 'helper' }
sub npe-calls-helper is export { npe-helper() }
