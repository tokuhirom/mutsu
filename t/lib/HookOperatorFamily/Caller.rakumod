unit module HookOperatorFamily::Caller;
sub call-it(&g) is export { g() }
