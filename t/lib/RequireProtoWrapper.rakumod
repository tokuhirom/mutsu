# A bare-file module whose exported routine occupies the GLOBAL namespace.
sub shared-name(|) is export { 'wrapper' }
