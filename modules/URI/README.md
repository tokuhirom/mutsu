[![Actions Status](https://github.com/raku-community-modules/URI/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/URI/actions) [![Actions Status](https://github.com/raku-community-modules/URI/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/URI/actions) [![Actions Status](https://github.com/raku-community-modules/URI/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/URI/actions)

NAME
====

URI — Uniform Resource Identifiers (absolute and relative)

SYNOPSIS
========

```raku
use URI;
my $u = URI.new('http://example.com/foo/bar?tag=woow#bla');

# Look at the parts
say $u.scheme;     #> http
say $u.authority;  #> example.com
say $u.host;       #> example.com
say $u.port;       #> 80
say $u.path;       #> /foo/bar
say $u.query;      #> tag=woow
say $u.fragment;   #> bla

# Modify the parts
$u.scheme('https');
$u.authority('example.com:8443');
$u.path('/bar/foo');
$u.query('x=1&y=2'); # OR
$u.fragment('cool');
say "$u"; #> https://example.com:8443/bar/foo?x=1&y=2#cool

# Authority is an object, but may be undefined
with $u.authority {
    say .userinfo; # none set, no output
    say .host;     #> example.com
    say .port;     #> 8443

    # It is mutable too
    .userinfo('bob');
    say $u.authority; #> bob@example.com:8443
}

# Path is an object, always defined, but immutable
with $u.path {
    say .path;          #> /bar/foo
    .say for .segments; #> bar\nfoo\n
}

# Query is an object, always defined, mutable
with $u.query {
    say .query;              #> x=1&y=2
    .query('foo=1&foo=2');
    say .query-form<foo>[0]; #> 1
    say .query-form<foo>[1]; #> 2

    .query-form.push: 'bar' => 'ok';
    say .query;              #> foo=1&foo=2&bar=ok

    .query('');
    .query-form<abc> = 123;
    say .query;              #> abc=123
}
```

DESCRIPTION
===========

A URI object represents a parsed string that abides by the grammar defined in RFC 3986 for Universal Resource Identifiers. The object then works to enforce the rules defined in the RFC for any modifications made to it.

As of this writing, The URI class is scheme agnostic. It will verify that URI is correct, in general, but not correct for a given scheme. For example, `http:foo` would be considered a valid URI even though that is not a valid URI according to the rules specific to the `http` scheme.

This class uses "heavy accessors". From the SYNOPSIS, you may have noted that assignment to accessors is not used. This is because nearly all accessors in this class do some extra work of parsing, validating, and cross-referencing with other fields to guarantee that the URI is correct before making a change.

```raku
my $u = URI.new;
$u.path = '/foo/bar'; # ERROR: «Cannot modify an immutable URI::Path»
$u.path('/foo/bar');  # WORKS!
```

This mutator pattern is meant to reflect the internal complexity.

SCHEME, AUTHORITY, AND PATH
---------------------------

In RFC 3986 URIs, the scheme, the authority, and the path are related. This should not matter most of the time, but to avoid problems when setting these three, it is safest to set them in this order:

```raku
my $u = URI.new;
$u.path('');
$u.scheme($my-scheme);
$u.authority($my-host);
$u.path($my-path);
```

This is because an empty path is permitted in any case, but the format of the path is limited whether an authority and scheme are set.

With an authority set (i.e., the URI either starts with "//" or with "scheme://"), a non-empty path must start with a "/" and may start with empty path segments, e.g. "scheme://foo//" is valid.

When there's no authority set, but a scheme is used (e.g., it starts with "scheme:", but not "scheme://"), a non-empty path may either start with a "/" or not, but must contain one or more other characters in the first segment of the path. Thus, the following code will fail:

```raku
my $u = URI.new('scheme:');
$u.path('//'); # ERROR: «Could not parse path "//" as part of URI: scheme://»
```

When there's no authority and no scheme used, a non-empty path may start with a "/" or not, but must contain one or more other characters in the first segment.

These rules are enforced whenever setting or clearing the scheme, authority, or path. If the resulting URI object would be invalid an `X::URI::Path::Invalid` exception will be thrown.

QUERY
-----

The `query` method of this class returns a `URI::Query` object. This is a special object that is `Positional`, `Associative`, and `Iterable`. That is, it can be bound to an array variable, a hash variable, and iterated using a loop. If stringified, it will return a URI-encoded query. If used as an array, will act like somewhat like an array of `Pair`s. If used as a hash, will provide a map from query keys to query values.

The native internal representation is a list of `Pair`s as this is the best match for how queries are defined. Because of this, there's a problem when using a query as a hash: duplicate pairs are valid. To handle this, the `URI::Query` object always returns a list of values, sorted in the order they appear for each key.

For example:

```raku
my $u = URI.new('?foo=1&bar=2&foo=3');
say $u.query<foo>[0]; #> 1
say $u.query<foo>[1]; #> 3
say $u.query<bar>[0]; #> 2
```

Older versions of the URI module handled this differently, using a mixed value representation. In order to gain some backwards compatibility, this is still supported by setting the `hash-format`:

```raku
# Continues from previous
$u.query.hash-format = URI::Query::Mixed;
say $u.query<foo>[0]; #> 1
say $u.query<foo>[1]; #> 3

# The bar value is NOT a list now
say $u.query<bar>;    #> 2

# Return to the default mode
$u.query.hash-format = URI::Query::Lists;
```

However, the list version is safer and may likely work with most existing code that worked with mixed before.

Another mode is provided to force single values, which is often how applications treat these out of convenience. In that case, only the last value will be kept:

```raku
# Continues from previous
$u.query.hash-format = URI::Query::Singles;

# These are never lists now
say $u.query<foo>; #> 3
say $u.query<bar>; #> 2
```

The `URI::Query::Lists` mode is default and recommended mode.

GRAMMAR
-------

This class will keep a copy of the result of parsing the URI string for you. If you are interested in precise details of the parse tree, you get them using the `grammar` method:

```raku
my $host-in-grammar =
  $u.grammar.parse-result<URI-reference><URI><hier-part><authority><host>;
if $host-in-grammar<reg-name> {
    say 'Host looks like registered domain name - approved!';
}
else {
    say 'Sorry we do not take ip address hosts at this time.';
    say 'Please use registered domain name!';
}
```

The `IETF::RFC_Grammar::URI` grammar sticks close to the BNF defined in RFC 3986.

PARTIAL MATCHING
----------------

Many times a URI you are interested in is embedded within another string. This class will allow you to parse URIs out of a larger string, so long as the URI is at the start. This is done by setting the `:match-prefix` option during construction or when calling `parse`:

```raku
{
    # require whole string matches URI and throw exception otherwise ..
    my $u_v = URI.new('http://?#?#');
    CATCH { when X::URI::Invalid { ... } }
}

my $u_pfx = URI.new('http://example.com } function(var mm){', :match-prefix);
```

METHODS
=======

method new
----------

```raku
multi method new(URI:U: Str() $uri, Bool :$match-prefix--> URI:D)
multi method new(URI:U: Str() :$uri, Bool :$match-prefix--> URI:D)
```

These construct a new `URI` object and return it. The given `$uri` value is converted to a string and then parsed using the `parse` method.

If `:match-prefix` is set, then the grammar will be allowed to match a prefix of the given input string rather than requiring a total match. The `:match-prefix` given also becomes the default value for any future calls to `parse`.

Throws a `X::URI::Invalid` exception if the URI cannot be parsed.

method parse
------------

```raku
method parse(URI:D: Str() $str, Bool :$match-prefix = $.match-prefix)
```

This method allows an existing URI object to be reused to parse another string. This parses the given string and replaces all internal state of the object with values for the new parse.

The given `:match-prefix` flag becomes the new default when set.

Throws a `X::URI::Invalid` exception if the URI cannot be parsed.

method grammar
--------------

```raku
method grammar(URI:D: --> IETF::RFC_Grammar:D)
```

Returns the object used to parse and store the state of the parse.

method match-prefix
-------------------

```raku
method match-prefix(URI:D:--> Bool:D)
```

Returns `True` if the most recent call to `parse` (or `new`) allowed a prefix match or `False` if a total match was required. This is the default value of any future call to `parse`.

method scheme
-------------

```raku
multi method scheme(URI:D:--> URI::Scheme:D)
multi method scheme(URI:D: Str() $scheme--> URI::Scheme:D)
```

Returns the scheme part of the URI. This is a string that must match the `URI::Scheme` subset type.

The second form allows the scheme to be replaced with a new scheme. It is legal for the scheme to be set to an empty string, which has the effect of making the URI scheme-less.

This will throw an `X::URI::Path::Invalid` exception if adding or removing the scheme will make the URI invalid. See SCHEME, AUTHORITY, AND PATH section for additional details.

method authority
----------------

```raku
multi method authority(URI:D:--> URI::Authority)
multi method authority(URI:D: Nil--> URI::Authority:U)
multi method authority(URI:D: Str() $new--> URI::Authority:D)
```

Returns the `URI::Authority` for the current URI object. This may be an undefined type object if no authority has been set or found during parse.

When passed a string, the string will have the authority parsed out of it and a new authority object will be used to store the parsed information. An empty authority is valid.

When passed `Nil`, the authority will be cleared.

When passing an argument, this will throw an `X::URI::Path::Invalid` exception if setting or clearing the authority on the URI will make the URI invalid. See SCHEME, AUTHORITY, AND PATH section for details.

The authority is made up of three components: userinfo, host, and port. Additional methods are provided for accessing each of those parts separately.

method userinfo
---------------

```raku
multi method userinfo(URI:D:--> URI::Userinfo:D)
multi method userinfo(URI:D: Str() $new--> URI::Userinfo:D)
```

The userinfo is an optional component of the URI authority. This method returns the current userinfo or an empty string.

Setting this method will cause a `URI::Authority` to be constructed and `authority` to be set, if it is not already defined. This may result in a `X::URI::Path::Invalid` exception being thrown if adding an authority will make the path invalid.

method host
-----------

```raku
multi method host(URI:D:--> URI::Host:D)
multi method host(URI:D: Str() $new--> URI::Host:D)
```

The host is a component of the URI authority. This method returns the current host or an empty string.

Setting this method will cause a `URI::Authority` to be constructed and `authority` to be set, if it is not already defined. This may result in a `X::URI::Path::Invalid` exception being thrown if adding an authority will make the path invalid.

method default-port
-------------------

```raku
method default-port(URI:D:--> URI::Port)
```

This method applies the `scheme-port` method of `URI::DefaultPort` to the scheme set on this object. Basically, a shortcut for:

```raku
my $u = URI.new("...");
my $port = URI::DefaultPort.scheme-port($u.scheme);
```

It returns the usual port for the named scheme.

method _port
------------

```raku
multi method _port(URI:D:--> URI::Port)
multi method _port(URI:D: Nil--> URI::Port:U)
multi method _port(URI:D: Int() $new--> URI::Port:D)
```

When an authority is set on the URI, this gets or sets the authority's port. This differs from `port`, which returns either the port set or the `default-port`. This method returns just the port.

If no authority is set or no port is set, this returns an undefined value (i.e., an `Int` type object).

Setting this method will cause a `URI::Authority` to be constructed and `authority` to be set, if it is not already defined. This may result in a `X::URI::Path::Invalid` exception being thrown if adding an authority will make the path invalid.

method port
-----------

```raku
multi method port(URI:D:--> URI::Port)
multi method port(URI:D: Nil--> URI::Port)
multi method port(URI:D: Int() $new--> URI::Port)
```

When retrieving a value from the object, this method returns either the port set on the authority or the default port for the current URI scheme. It may return an undefined value (i.e., an `Int` type object) if there is no port set and no known default port for the current scheme or no scheme set.

When setting a value on the object, this method sets the port on the authority.

Setting this method will cause a `URI::Authority` to be constructed and `authority` to be set, if it is not already defined. This may result in a `X::URI::Path::Invalid` exception being thrown if adding an authority will make the path invalid.

method path
-----------

```raku
multi method path(URI:D:--> URI::Path:D)
multi method path(URI:D: Str() $path--> URI::Path:D)
```

Path is the main required element of a URI, but may be an empty string. This method returns the current setting for the path as a `URI::Path` object. It also allows setting a new path, which will construct a new `URI::Path` object.

This method will throw a `X::URI::Path::Invalid` exception if the path is not valid for the current scheme and authority settings.

method segments
---------------

```raku
multi method segments(URI:D:--> List:D)
multi method segments(URI:D: @segments where *.elems > 0--> List:D)
multi method segments(URI:D: $first-segment, *@remaining-segments--> List:D)
```

Returns the path segments (i.e., the parts between slashes). This is a shortcut for:

```raku
my $u = URI.new("...");
my @s = $u.path.segments;
```

The number of segments is equal to the number of slashes in the original path plus one. The segments are constructed so that joining the segments by a slash (/) will give you the original path.

You may pass a list in to replace the segments with a new value. This will throw a `X::URI::Path::Invalid` exception if any of the segments contain a slash or if the set path will cause the URI to become invalid.

Be sure when setting segments to include an initial empty string if you want the path to start with a slash.

method query
------------

```raku
multi method query(URI:D:--> URI::Query:D)
multi method query(URI:D: Str() $new--> URI::Query:D)
multi method query(URI:D: *@new--> URI::Query:D)
```

Accesses or updates the query associated with the URI. This is returns as a `URI::Query` object. This will always be defined, but may be empty.

When passed a string, the string will be parsed using `split-query` and the query object will be updated. When passed a list, the list of `Pair`s given will be used to setup a new query that way.

When the list form is used, any named parameters passed will be ignored (they will not be used to fill the query) and a warning will be issued.

method path-query
-----------------

```raku
method path-query(URI:D:--> Str:D)
```

Returns the path and query as a string joined by a question mark ("?"). It will return just the path as a string if the query is empty.

method fragment
---------------

```raku
multi method fragment(URI:D:--> URI::Fragment:D)
multi method fragment(URI:D: Str() $new--> URI::Fragment:D)
```

Returns the URI fragment, which is always defined, but may be an empty string. If passed a value, it will set the fragment.

method gist
-----------

```raku
multi method gist(URI:D:--> Str:D)
```

Reconstructs the URI from the components and returns it as a string.

method Str
----------

```raku
multi method Str(URI:D:--> Str:D)
```

Reconstructs the URI from the components and returns it as a string.

SUBROUTINES
===========

sub split-query
---------------

```raku
sub split-query(Str() $query, :$hash-format = URI::Query::None)
```

This routine will slice and dice a query string, which is useful when parsing URIs and may also be useful when parsing POST entities that are encoded as `application/x-www-form-urlencoded`.

This routine is exported with the `:split-query` tag or can be used with the full namespace, `URI::split-query`.

With just the required string, this routine will parse that string and return a list of `Pair`s mapping each query form key to its respective value. The order is preserved. This is used by `URI::Query` during construction.

For example:

```raku
my @qf = URI::split-query("foo=1&bar%20=2&foo=3");
dd @qf; #> Array @qf = [:foo("1"), "bar " => ("2"), :foo("3")]
```

Notice that this will perform a `uri-escape` operation of keys and values in the process so the values you receive have had the URI encoded characters decoded.

You can retrieve the query string as a hash instead by passing the `:hash-format` option. This works exactly as it does for `URI::Query`.

The options for `:hash-format` include:

### URI::Query::Lists

Every key is mapped to a list of one or more values. From the example input, a structure like the following is returned:

```raku
my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
    hash-format => URI::Query::Lists,
);
dd %qf; #> Hash %qf = {"bar " => (["2"]), :foo(["1", "3"])}
```

This is also the default if you just pass `:hash-format` as a boolean option.

### URI::Query::Mixed

Every key is mapped to either a list of two or more values or directly to a single value, like the following:

```raku
my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
    hash-format => URI::Query::Mixed,
);
dd %qf; #> Hash %qf = {"bar " => ("2"), :foo(["1", "3"])}
```

### URI::Query::Singles

Every key is mapped to a single value, which will be the last value encountered in the input, like this:

```raku
my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
    hash-format => URI::Query::Mixed,
);
dd %qf; #> Hash %qf = {"bar " => ("2"), :foo("3")}
```

HELPER SUBSETS
==============

URI::Scheme
-----------

This is a subset of `Str` that only accepts valid schemes.

URI::Userinfo
-------------

This is a subset of `Str` that only accepts valid userinfo.

URI::Host
---------

This is a subset of `Str` that only accepts valid hosts.

URI::Port
---------

This is a subset of `UInt`.

URI::Query::ValidQuery
----------------------

This is a subset of `Str` that only accepts valid query strings.

URI::Fragment
-------------

This is a subset of `Str` that only accepts valid URI fragments.

HELPER CLASSES
==============

URI::Authority
--------------

The `authority` method of a URI constructs or returns this object.

It is recommended that you do not construct a `URI::Authority` object directly, but let the methods of `URI` handle construction.

### method userinfo

```raku
method userinfo(URI::Authority:D: --> URI::Userinfo:D) is rw
```

This is a simple setter/getter for the userinfo on the authority. It must be defined, but may be the empty string. If not empty, it must be valid for the userinfo component of a URI.

### method host

```raku
method host(URI::Authority:D: --> URI::Host:D) is rw
```

This is a simple setter/getter for the host part of the URI authority. It must be defined, but may be the empty string. If not empty, must be a valid host value, which may be an IP address or registered name.

### method port

```raku
method port(URI::Authority:D: --> URI::Port) is rw
```

This is a simple setter/getter for the port part of the URI authority. It may be set to an undefined value if no explicit port is set in the authority. If defined, it must an unsigned integer.

### method gist

```raku
multi method gist(URI::Authority:D:--> Str:D)
```

Returns the string representation of the URI authority. For example:

```raku
my $u = URI.new("http://steve@example.com:8008");

# say calls .gist
say $u.authority; #> "steve@example.com:8080";
```

### method Str

```raku
multi method gist(URI::Authority:D:--> Str:D)
```

Stringifies identical to `gist`.

URI::Path
---------

This class is used to represent URI path components.

It is recommended that you do not construct a `URI::Path` object directly, but rely on the `path` setter in `URI` instead. These objects are immutable. Please use methods on `URI` to make changes.

### method path

```raku
method path(URI::Path:D:--> Str:D)
```

Returns the string representation of the path.

### method segments

```raku
method segments(URI::Path:D:--> List:D)
```

Returns a list representation of the path segments. In a URI, the path segments are the strings between slashes ("/").

### method gist

```raku
method gist(URI::Path:D:--> Str:D)
```

Returns the `path`.

### method Str

```raku
method Str(URI::Path:D:--> Str:D)
```

Returns the `path`.

URI::Query
----------

This class is used to represent a URI query component. This class may be safely constructed and used independently of the URI object.

It behaves as both a positional and associative object and is iterable. Internally, it is stored as an `Array` of `Pair`s. You must not treat this object purely as you would an `Array` or purely as you would a `Hash` as some methods will not work the way you expect.

The performance of the associative methods is not guaranteed and is probably going to be relatively slow. This implementation values simplicity and accuracy of representation to CPU performance. If you need something that is better for CPU performance, you should investigate the use of another library, such as `Hash::MultiValue` or sub-class to provide a higher performance implementation of `URI::Query`'s associative methods.

### method new

```raku
multi method new(Str() $query, URI::Query::HashFormat :$hash-format = URI::Query::Lists--> URI::Query:D)
multi method new(:$query, URI::Query::HashFormat :$hash-format = URI::Query::Lists--> URI::Query:D)
```

Constructs a new `URI::Query` from the given string, which may be empty.

Unlike `split-query`, which permits boolean values for the `hash-format` option, `URI::Query` requires a `URI::Query::HashFormat` value and will reject a boolean (because the boolean does not make sense in this case).

### enum HashFormat

This enumeration provides the values that may be set on `hash-format` on a `URI::Query`. Possible values are as follows.

#### URI::Query::List

This is the default and recommended value. When set in `hash-format`, each key in the hash representation will point to a list of one or more values.

This is recommended as it is the most accurate representation to what is actually possible in a query string. The values within each key will be sorted in the same order as they appear in the query.

#### URI::Query::Mixed

When set in `hash-format`, each key in the hash representation may be mapped to either a list of two or more values, or to the value itself if there is only one.

This is not recommended because it means that setting a key to an iterable value will be treated as multiple key-value pairs when it comes time to render the query as a string. This could have unintended consequences.

#### URI::Query::Singles

When set in `hash-format`, each key in the hash representation will be mapped directly to a single value. If there are multiple values that have been set in the query, then only the last will be visible through the hash representation.

This is not recommended because it may hide certain values, but may be useful for simple applications that treat the query string as having unique values. Just note that the trade-off is that your application may be confused when multiple values are present.

#### URI::Query::None

This value should not be used, but will be treated the same as `URI::Query::List` if used here. It is provided for use with `split-query` only.

### method hash-format

```raku
method hash-format(URI::Query:D --> URI::Query::HashFormat) is rw
```

This is a simple setter/getter for setting the way in which associative lookups are performed. See `enum URI::Query::HashFormat` for a description of each mode.

### method query

```raku
method query(Query:D:--> URI::Query::ValidQuery:D)
method query(Query:D: Str() $new--> URI::Query::ValidQuery:D)
```

This method returns the string representation of the URI query component. When passed a string, it will replace the query with the new value and will use `split-query` to parse that query into an array of `Pair`s.

The primary representation of the queryo object is the value returned by `query-form`. The `query` is cached to keep the value stored in it when this method is called to set it or when `URI::Query` is constructed from a string. Any modification to the internal array of pairs, though, will clear this cache. It will be generated the next time the `query` method is called and that string will be cached.

### method query-form

```raku
method query-form(Query:D:--> Array:D)
method query-form(Query:D: *@new, *%new--> Array:D)
```

This method returns the array of `Pair`s that store the internal representation of the URI query component. When passed an array of `Pair`s, it will replace the current value with that array.

A quick note about the way pairs are passed as parameters to a method, you most likely want to avoid passing values as named parameters. If values are passed using unquoted strings, they will be treated as named parameters, which is most likely what you want:

```raku
my $q = URI::Query.new;

# Prefer this
$q.query-form('foo' => 1, 'bar' => 2, 'foo' => 3);

# Avoid these
$q.query-form(foo => 1, bar => 2, foo => 3);
$q.query-form(:foo(1), :bar(2), :foo(3));
```

The latter two will result in the first "foo" pair being lost. Named parameters assume unique names and the latter "foo" key will effectively override the former.

That said, the method will allow hashes to be passed in, if that is your preference.

### method of

```raku
method of()
```

Always returns `Pair`.

### method iterator

```raku
method iterator(Query:D:--> Iterator:D)
```

Returns the iterator on the internal array of `Pair`s.

### method postcircumflex:<[ ]>

```raku
method postcircumflex:<[ ]> returns Pair
```

This permits positional access to each `Pair` stored internally. You may use this to get a `Pair`, set a `Pair`, test for existence, or delete.

### method postcircumflex:<{ }>

```raku
method AT-KEY(key)
```

This permits associative access to the values stored internally by key. What is returned here when fetching values depends on the setting in `hash-format`, a list of one or more values or `Nil`, by default. You can use this for getting, setting, existence testing, or deletion.

### method keys

```raku
method keys(Query:D:--> Seq:D)
```

This method returns all the keys of the query in order.

### method values

```raku
method values(Query:D:--> Seq:D)
```

This method returns all the values of the query in order.

### method kv

```raku
method kv(Query:D:--> Seq:D)
```

This method returns a sequence alternating the keys and values of the query in order.

### method pairs

```raku
method kv(Query:D:--> Seq:D)
```

This method returns a copy of the internal representation of the query string array.

### method pop

```raku
method pop(Query:D:--> Pair)
```

This method removes the last `Pair` from the array of pairs and returns it.

### method push

```raku
method push(Query:D: *@new)
```

This method adds the given pairs to the end of the array of pairs in the query using `push` semantics.

### method append

```raku
method append(Query:D: *@new)
```

This method adds the given pairs to the end of the array of pairs in the query using `append` semantics.

### method shift

```raku
method shift(Query:D:--> Pair)
```

This method removes the first `Pair` from the array of pairs and returns it.

### method unshift

```raku
method unshift(Query:D: *@new)
```

This method adds the given pairs to the front of the array of pairs in the query using `unshift` semantics.

### method prepend

```raku
method prepend(Query:D: *@new)
```

This method adds the given pairs to the front of the array of pairs in the query using `prepend` semantics.

### method splice

```raku
method splice(Query:D: $start, $elems?, *@replacement)
```

This method removes a `$elems` number of pairs from the array of pairs in the query starting at index `$start`. It then inserts the pairs in `@replacement` into that part of the array (if any are given).

### method elems

```raku
method elems(Query:D:--> Int:D)
```

Returns the number of pairs stored in the query.

### method end

```raku
method end(Query:D:--> Int:D)
```

Returns the index of the last pair stored in the query.

### method Bool

```raku
method Bool(Query:D:--> Bool:D)
```

Returns `True` if the at least one pair is stored in the query or `False` otherwise.

### method Int

```raku
method Int(Query:D:--> Int:D)
```

Returns `elems`.

### method Numeric

```raku
method Numeric(Query:D:--> Int:D)
```

Returns `elems`.

### method gist

```raku
method gist(Query:D:--> Str:D)
```

Returns the `query`.

### method Str

```raku
method Str(Query:D:--> Str:D)
```

Returns the `query`.

EXCEPTIONS
==========

X::URI::Invalid
---------------

This exception is thrown in many places where the URI is being parsed or manipulated. If the string being parsed is not a valid URI or if certain manipulations of the URI object would cause it to become an invalid URI, this exception may be used.

It provides a `source` accessor, which returns the string that was determined to be invalid.

X::URI::Path::Invalid
---------------------

In some cases where an attempt is made to set `path` to an invalid value, this exception is thrown.

The `source` field will name the invalid URI. Strictly speaking, the URI might be valid, but will not parse the same way as given. To make it clear that this is the case, the `path` field names the invalid path part.

In cases where the segments have been modified in an invalid way, the first invalid segment will be set in `bad-segment`.

POTENTIALLY INCOMPATIBLE CHANGES BETWEEN v0.2.2 and v0.3.0
==========================================================

The v0.3.0 introduced the ability to mutate the parts of an existing URI, which in turn may have introduced changes which are incompatible with existing applications:

  * URI.query now returns an object of type URI::Query not a Str The object will coerce correctly when interpolated into a string but will need an explicit .Str coercion if being assigned to a Str typed variable.

  * URI.path now returns an object of type URI::Path not a Str The object will coerce correctly when interpolated into a string but will need an explicit .Str coercion if being assigned to a Str typed variable.

  * URI.query-form no longer returns a Hash but an object of URI::Query The object does the Associative role so for the most part can be treated like a Hash but an explicit .Hash coercion may be required when comparing with another Hash or when merging with another Hash. Some uses of query-form have been marked as deprecated and should use .query instead.

The changes have been tested with the majority of modules that depend on URI and only a few required changes.

AUTHORS
=======

Contributors to this module include:

  * Ronald Schmidt (ronaldxs)

  * Moritz Lentz (moritz)

  * Nick Logan (ugexe)

  * Tobias Leich (FROGGS)

  * Jonathan Stowe (jonathanstowe)

  * Justin DeVuyst (jdv)

  * Solomon Foster (colomon)

  * Roman Baumer (rba)

  * Zoffix Znet (zoffixznet)

  * Ahmad M. Zawawi (azawawi)

  * Gabor Szabo (szabgab)

  * Samantha McVey (samcv)

  * Pawel Pabian (bbkr)

  * Rob Hoelz (hoelzro)

  * radiak

  * Paul Cochrane (paultcochrane)

  * Steve Mynott (stmuk)

  * timo

  * David Warring (dwarring)

  * Sterling Hanenkamp (zostay)

Source can be located at: https://github.com/raku-community-modules/URI . Comments and Pull Requests are welcome.

COPYRIGHT & LICENSE
===================

Copyright 2017 Ronald Schmidt.

Copyright 2015 - 2024 Raku Community Authors

This software is licensed under the Artistic 2.0 License.

