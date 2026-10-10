use Test;
# From URI::Query::FromHash: an optional `&c = ...` parameter must not make
# `Any:D $_, &c = ...` outrank the narrower `Bool:D $_, |c`.
plan 5;

multi esc(Any:U $, |c --> '') { }
multi esc(Bool:D $_, |c) { 'bool' }
multi esc(Any:D $_, &class = &say) { 'any' }
multi esc(Blob:D $_, |c) { 'blob' }

is esc(True), 'bool', 'Bool:D beats Any:D with optional &param';
is esc('a'.encode), 'blob', 'Blob:D beats Any:D with optional &param';
is esc(5), 'any', 'Int falls to the Any:D candidate';
is esc(Int), '', 'type object hits the :U candidate';

multi req(Any:D $_, &c) { 'any' }
multi req(Bool:D $_, &c) { 'bool' }
is req(True, &say), 'bool', 'required &param still ranks';
