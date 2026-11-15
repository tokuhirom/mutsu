# Ecosystem sweep retains private caches within each distribution run

The ecosystem sweep now gives mutsu and rakudo separate private cache
directories that persist across one distribution's test files. Each directory
is mounted under the sandbox's throwaway HOME and removed with the
distribution workdir. This lets mutsu reuse its precompiled modules after the
load probe, as rakudo already does through the writable distribution copy.
Both interpreters now run untimed load probes before the first test file,
without sharing a cache across interpreters or distributions (#17688).
The remaining warm-cache performance gap in SBOM::CycloneDX's validation
workload is tracked separately in #17811.
