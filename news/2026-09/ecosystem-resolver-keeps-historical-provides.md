# Ecosystem resolver retains historical module providers

The ecosystem sweep now builds its module-to-distribution map from every
version in the merged fez and REA indexes, while keeping the latest
distribution entry for the dependency pool. This preserves dependencies such
as `Digest::SHA`, which is provided by older `Digest` releases but no longer
listed by the current release, and prevents them from being misclassified as
`blocked_dep`.

The resolver self-test covers the `Digest::SHA` case from #8685.

Closes #8685.
