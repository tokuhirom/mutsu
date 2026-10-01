unit module NowTimeShadowFixture;

our sub now(--> Str) is export { 'imported now' }
our sub time(--> Int) is export { 42 }
