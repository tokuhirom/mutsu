unit module EndPhaserImportProvider;

multi sub shout(Cool:D $s is copy) { shout $s.IO }
multi sub shout(IO::Path:D $p) is export { say "end saw: $p" }
