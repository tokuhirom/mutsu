use Test;
use NativeCall;

# A native routine whose return type is a `constant` alias of a CStruct class
# (`--> PwStruct` with `my constant PwStruct = PwStructLinux`, P5getpwnam's
# per-OS pick) returns an instance of the aliased class, whose methods work.
# It used to be tagged with the alias name, so every method was missing.

plan 4;

my class PwLinux is repr<CStruct> {
    has Str    $.pw_name;
    has Str    $.pw_passwd;
    has uint32 $.pw_uid;
    has uint32 $.pw_gid;
    method who { $.pw_name }
}
my constant PwAlias = PwLinux;

sub getpwuid_aliased(uint32 --> PwAlias) is native is symbol<getpwuid> {*}
sub getpwuid_direct(uint32 --> PwLinux) is native is symbol<getpwuid> {*}

my $aliased = getpwuid_aliased(0);
my $direct  = getpwuid_direct(0);

is $aliased.^name, $direct.^name, 'the alias returns the same class as the direct spelling';
ok $aliased ~~ PwLinux, 'it is an instance of the aliased class';
is $aliased.who, $direct.who, "the class's own method works";
is $aliased.pw_uid, 0, 'and its fields read through';
