# NativeLibs

The simple use in your module is:

```perl6
use NativeLibs; # This also re-exports NativeCall :DEFAULTS for convenience
my $Lib; # To keep the reference

sub some_native_func() is native { * } # Note no library needed
… The rest of your module

INIT {
    # Load the needed library
    without $Lib = NativeLibs::Loader.load('libsomelib.so.4') {
        .fail;
    }
}
…
```

If in your native library binding you need to support a range of versions:

```perl6
use NativeLibs;

constant LIB = NativeLibs::Searcher.at-runtime(
    'mysqlclient', # The library short name
    'mysql_init',  # A 'well known symbol'
    16..20	   # A List of supported versions, a range in this example
);

sub mysql_get_client_info(--> Str)       is export is native(LIB) { * }

...
```

This is a grow-up version of the original NativeLibs (v0.0.3) included in DBIish now released
to allow the interested people the testing and discussion of the module.

So, if you use this in your own module, please use with a version, for example:

```perl6
use NativeLibs:ver<0.0.5>;
```

and include `"NativeLibs:ver<0.0.5+>"` in your META6's depends

Other examples in the drivers of https://github.com/perl6/DBIish
