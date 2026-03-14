use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 17;

my $dir = make-temp-dir();
my $file = make-temp-file(:content("hello"), :chmod(0o644));
my $exe = make-temp-file(:content("#!/bin/sh\n"), :chmod(0o755));
my $missing = $dir.child("missing-file-test-ops");

ok $dir.e, ".e returns True for an existing directory";
ok $dir.d, ".d returns True for a directory";
nok $dir.f, ".f returns False for a directory";

ok $file.e, ".e returns True for an existing file";
ok $file.f, ".f returns True for a regular file";
nok $file.d, ".d returns False for a regular file";
is $file.s, 5, ".s returns the file size";
ok $file.r, ".r returns True for a readable file";
ok $file.w, ".w returns True for a writable file";
ok $exe.x, ".x returns True for an executable file";

ok !$missing.e, ".e returns False for a missing path";
throws-like { $missing.d }, X::IO::DoesNotExist,
    ".d throws X::IO::DoesNotExist for a missing path";
throws-like { $missing.f }, X::IO::DoesNotExist,
    ".f throws X::IO::DoesNotExist for a missing path";
throws-like { $missing.r }, X::IO::DoesNotExist,
    ".r throws X::IO::DoesNotExist for a missing path";
throws-like { $missing.w }, X::IO::DoesNotExist,
    ".w throws X::IO::DoesNotExist for a missing path";
throws-like { $missing.x }, X::IO::DoesNotExist,
    ".x throws X::IO::DoesNotExist for a missing path";
throws-like { $missing.s }, X::IO::DoesNotExist,
    ".s throws X::IO::DoesNotExist for a missing path";
