# Slang token and action output reaches stderr

Output written by a slang token or action while the parser runs its host was
left in that host's private buffer. The host now sends its output to the
process streams, so `note` and `$*ERR.say` appear during compilation. Actions
that run again in the importing unit to restore interpreter-local effects no
longer print the same message twice. This applies only to an executing parse;
the analysis paths still skip slang activation.
