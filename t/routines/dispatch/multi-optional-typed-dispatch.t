use Test;

plan 1;

multi sub choose(&code) { 'helper' }
multi sub choose(Blob $payload, Blob $extra?) { 'blob' }

is choose(Buf.new(0x41)), 'blob', 'typed optional candidate wins over helper multi';
