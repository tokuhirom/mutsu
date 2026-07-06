unit module TraitParamOtf;

# Module subs with standard binding-time param traits (is copy / is rw / is raw
# / is readonly / is required). Before the §2 gate relaxation these were forced
# onto the interpreter fallback by `pd.traits.is_empty()` in
# def_is_otf_compilable_module_single, even though the compiled binding path
# already handled them (def_is_otf_compilable, the builtin-shadow gate, never
# checked param traits). The interpreter fallback also silently dropped the
# `is rw` caller writeback; OTF-compiling fixes it (compile-time caller slot).

# is copy: a fresh writable local copy, no writeback to the caller.
sub bump-copy($n is copy) is export { $n++; $n * 2 }

# is rw: writes back to the caller's variable.
sub set-rw($x is rw) is export { $x = 99 }

# is raw: binds the argument without decontainerizing.
sub raw-id($x is raw) is export { $x }

# is readonly (explicit): same as the default binding.
sub ro-id($x is readonly) is export { $x + 1 }

# is required (redundant with positional, but exercises the trait).
sub req-id($x is required) is export { $x * 3 }

# rw with a default value (name-cache-safe for a non-builtin module sub).
sub rw-or($x is rw, $bump = 1) is export { $x = $x + $bump }
