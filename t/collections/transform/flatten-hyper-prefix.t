use Test;

plan 1;

my @chunks;
is (Blob.new: |«@chunks).gist, 'Blob:0x<>', '|« can be used as a call argument prefix';
