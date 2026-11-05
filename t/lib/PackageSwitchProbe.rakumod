unit module PackageSwitchProbe;

sub where-am-i() is export { $?PACKAGE.^name }

sub call-back(&cb) is export { cb() }
