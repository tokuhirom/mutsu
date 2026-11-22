use v6.d;
unit module ClientRevision6d;
# Calls back into code of another package (#18742).
sub call-back(&code) is export { code }
sub my-client-rev is export { CLIENT::LEXICAL::<CORE-SETTING-REV> }
sub my-client-pkg is export { CLIENT::LEXICAL::<$?PACKAGE> }
