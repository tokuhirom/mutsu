unit module ModuleScopeSymbolDefs;

# An exported enum whose key is spelled with no sigil, and an exported
# sigil-less constant. Both are read by their BARE name from the importing
# module's own routines (see ModuleScopeSymbolUser).
our Str enum MSSEnum is export(:MSS) « :mssz<ZZVAL> »;
constant mss-const is export(:MSS) = 'CONSTVAL';
