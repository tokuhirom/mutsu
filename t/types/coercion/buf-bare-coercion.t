use Test;

# Protocol::Postgres uses bare Blob(...) calls in its packet fixtures. They
# must use the same native byte construction as Blob.new(...).
plan 3;

is-deeply Blob(0).list, (0,), 'bare Blob coercion builds one byte';
is-deeply Blob(1, 2, 3).list, (1, 2, 3), 'bare Blob coercion builds bytes';
is-deeply Buf(1, 2).list, (1, 2), 'bare Buf coercion builds bytes';
