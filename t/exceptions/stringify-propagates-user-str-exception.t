use Test;

plan 5;

# An exception raised by a user `.Str` is the result of stringifying the
# object; only a MISSING method falls back to the default `Type<id>` form.
class Boom { method Str { die "no string for you" } }

throws-like { ~Boom.new }, X::AdHoc, message => 'no string for you',
    'prefix ~ propagates the exception';
throws-like { "{Boom.new}" }, X::AdHoc, message => 'no string for you',
    'interpolation propagates the exception';

my $s = try { ~Boom.new };
nok $s.defined, 'try around ~ yields Nil';
ok $! ~~ X::AdHoc, '... and sets $!';

# The Syndicate shape: `.Str` locks and dies on an invalid feed.
class Feed {
    has $!lock = Lock.new;
    method Str { $!lock.protect: { die "Atom feed requires 'updated' timestamp" } }
}
throws-like { ~Feed.new }, X::AdHoc, message => /updated/,
    'an exception from inside Lock.protect in .Str propagates';
