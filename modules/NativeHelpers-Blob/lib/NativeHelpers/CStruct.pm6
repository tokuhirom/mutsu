use v6;

unit module NativeHelpers::CStruct:ver<0.1.2>;
use NativeCall;
use MoarVM::Guts::REPRs;
#use nqp;
constant stdlib = Rakudo::Internals.IS-WIN ?? 'msvcrt' !! Str;
our $debug = False;

role LinearArray[::T] does Positional[T] is export {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    my int $sol = nativesizeof(T);
    my \ty = T;

    has Pointer $!storage;
    has @!cache handles <AT-POS elems shape>;
    has Int $!size;
    has $.managed;

    sub calloc(size_t, size_t --> Pointer) is native(stdlib) { * }
    submethod BUILD(:$!size!, :$!storage!, :$!managed) {
	@!cache := Array[ty].new(:shape($!size));
	for ^$!size {
	    my Pointer $p .= new(+$!storage + $_ * $sol);
	    @!cache[$_] = nativecast(T, $p);
	}
	self;
    }

    method new(::?CLASS:U: Int $size) {
	with calloc($size, $sol) -> $storage {
	    self.bless(:$size, :$storage, :managed);
	} else {
	    fail "Can't allocate memory";
	}
    }

    method new-from-pointer(::?CLASS:U: Int :$size, Pointer :$ptr) {
	self.bless(:$size, :storage(nativecast(Pointer,$ptr)), :!managed);
    }

    sub free(Pointer) is native(stdlib) { * }
    method dispose(::?CLASS:D:) {
	with $!storage {
	    @!cache := ();
	    free($!storage) if $!managed;
	    $!storage = Pointer;
	    True;
	} else {
	    False;
	}
    }

    method nativesizeof() {
	$sol * $!size;
    }

    multi method Pointer(::?CLASS:D: :$typed) {
	$typed ?? nativecast(Pointer[ty],$!storage) !! $!storage;
    }

    method base() {
	@!cache[0];
    }
    # Back-compat for DBIish's mysql
    method typed-pointer() {
	@!cache[0];
    }

    method _Pointer(Int $idx) {
	BODY_OF(@!cache[$idx]).cstruct;
    }

    multi method Pointer(::?CLASS:U: T:D $struct) {
	BODY_OF($struct).cstruct;
    }
}

multi sub pointer-to(Mu:D $struct where .REPR eq 'CStruct', :$typed) is export {
    my \t = $struct.WHAT;
    my $sb = BODY_OF($struct);
    note "From ", $sb.perl if $debug;
    my \ptr = $sb.cstruct;
    $typed ?? nativecast(Pointer[t], ptr) !! ptr;

}
