unit class ExportedCodeVarHashUnit is Hash;
use ExportedCodeVarProvider :pack;

method method-form($b) { $b.&unpack(5) }
method call-form($b) { unpack($b, 6) }
method code-var-name() { &unpack.name }
