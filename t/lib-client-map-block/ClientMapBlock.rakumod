use v6.d;
unit module ClientMapBlock;

sub callback-simple(&code) is export { code }

sub callback-in-map(&code) is export { <a>.map: { code } }

sub callback-in-grep(&code) is export { <a>.grep: { code; True } }

sub package-in-map is export { <a>.map({ $?PACKAGE }).List }
