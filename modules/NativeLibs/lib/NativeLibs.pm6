use v6;

use NativeCall;
sub EXPORT(|) {
    my $exp = &trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) };
    Map.new(
        'NativeCall' => NativeCall,
        '&trait_mod:<is>' => $exp.dispatcher
    )
}
unit module NativeLibs:auth<salortiz>:ver<0.0.9>;

our constant is-win = Rakudo::Internals.IS-WIN();

our proto sub cannon-name(|) { * }
multi sub cannon-name(Str $libname, Version $version = Version) {
    with $libname.IO {
	if .extension {
	    .Str; # Assume resolved, so don't touch
	} else {
	    $*VM.platform-library-name($_, :$version).Str;
	}
    }
}
multi sub cannon-name(Str $libname, Cool $ver) {
    cannon-name($libname, Version.new($ver));
}

class Loader {
    # This is an HLL clone of MoarVM's loadlib, freelib, et.al. ops.
    # not available in rakudo.
    class DLLib is repr('CPointer') { };

    my  %Libraries;
    my  \dyncall = $*VM.config<nativecall_backend> eq 'dyncall';
    constant k32 = 'kernel32'; # Main windows dll

    has Str   $.name;
    has DLLib $.library;

    sub dlerror(--> Str)         is native { * } # For linux or darwin/OS X
    sub GetLastError(--> uint32) is native(k32) { * } # On Microsoft land
    method !dlerror() {
        is-win ?? "error({ GetLastError })" !! (dlerror() // '');
    }

    sub dlLoadLibrary(Str --> DLLib)  is native { * } # dyncall
    sub dlopen(Str, uint32 --> DLLib) is native { * } # libffi
    sub LoadLibraryA(Str --> DLLib) is native(k32) { * }
    method !dlLoadLibrary(Str $libname --> DLLib) {
	is-win  ?? LoadLibraryA($libname) !!
        dyncall ?? dlLoadLibrary($libname) !!
        dlopen($libname, 0x102); # RTLD_GLOBAL | RTLD_NOW

    }

    method load(::?CLASS:U: $libname) {
	with self!dlLoadLibrary($libname) {
	    self.bless(:name($libname), :library($_));
	} else {
	    fail "Cannot load native library '$libname'";
	}
    }

    sub dlFindSymbol(  DLLib, Str --> Pointer) is native { * } # dyncall
    sub dlsym(         DLLib, Str --> Pointer) is native { * } # libffi
    sub GetProcAddress(DLLib, Str --> Pointer) is native(k32) { * }
    sub GetModuleHandleA(     Str -->   DLLib) is native(k32) { * }
    method symbol(::?CLASS $self: Str $symbol, Mu $want = Pointer) {
	my \c = \(
            $self.DEFINITE ?? $!library !!
            is-win ?? GetModuleHandleA(Str) !! DLLib,
            $symbol
        );
	with (
            is-win ?? &GetProcAddress !! dyncall ?? &dlFindSymbol !! &dlsym
        )(|c) {
            if $want !=== Pointer {
                nativecast($want, $_);
            } else {
                $_
            }
        } else {
            fail "Symbol '$symbol' not found";
        }
    }

    sub dlFreeLibrary(DLLib) is native { * }
    sub dlclose(      DLLib) is native { * }
    sub FreeLibrary(  DLLib --> int32) is native(k32) { * }
    method dispose(--> True) {
	with $!library {
            is-win  ?? FreeLibrary($_) !!
	    dyncall ?? dlFreeLibrary($_) !! dlclose($_);
	    $_ = Nil;
	}
    }
}

class Searcher {
    method !test($try, $wks) {
	(try cglobal($try, $wks, Pointer)) ~~ Pointer ?? $try !! Nil
    }
    method try-versions(Str $libname, Str $wks, *@vers) {
	my $wlibname;
	for @vers {
	    my $version = $_.defined ?? Version.new($_) !! Version;
	    $wlibname = $_ and last with self!test:
		cannon-name($libname, $version), $wks;
	}
	# Try unversionized
	$wlibname //= self!test: cannon-name($libname), $wks unless @vers;
	# Try common practice in Windows;
	$wlibname //= self!test: "lib$libname.dll", $wks if is-win;
	$wlibname;
    }

    method at-runtime($libname, $wks, *@vers) {
	-> {
	    with self.try-versions($libname, $wks, |@vers) {
		$_
	    } else {
		# The sensate thing to do is die, but somehow that don't work
		#   ( 'Cannot invoke this object' ... )
		# so let NC::!setup die for us returning $libname.
		#die "Cannot locate native library '$libname'"
		$libname;
	    }
	}
    }
}

class Compile {
    has @.files;
    has $.name;
    has $.lib;
    has $.outdir;

    my $cfg = $*VM.config;
    submethod BUILD(:$!name!, :$!outdir, :@!files) {
        @!files.push: $!name unless @!files;
        $!lib = $*VM.platform-library-name($!name.IO);
        $_ .= subst(/\.c$/,'') for @!files;
    }

    method compile-file($file is copy) {
        my $CC = "$cfg<cc> -c $cfg<ccshared> $cfg<cflags>";
        my $c-line = join(' ', $CC, "$cfg<ccout>$file$cfg<obj>", "$file.c");
        shell($c-line);
    }

    method compile-all {
        self.compile-file($_) for @!files;
        my $lds = is-win ?? '' !! $cfg<ldshared>;
        my $LD = "$cfg<ld> $lds $cfg<ldflags> $cfg<ldlibs>";
        my $l-line = join(' ', $LD, "$cfg<ldout>$!lib", @!files.map(* ~ $cfg<obj>));
        shell($l-line);
    }
}

# Reexport on demand all of NativeCall
CHECK for NativeCall::EXPORT::.keys {
    UNIT::EXPORT::{$_} := NativeCall::EXPORT::{$_};
}
# vim: ft=perl6:st=4:sw=4:et
