unit module IOBuiltinShadow;

my @log;

#| Exported under a name that collides with the `put` IO builtin, the way
#| Cro::HTTP::Router exports the HTTP verb `put`.
multi put(&handler --> Nil) is export { @log.push('put') }

sub note-it(Str $x --> Nil) is export { @log.push('note-it') }

sub shadow-log() is export { @log.join(',') }
