# Remote sessions offer to start the next session themselves

A session that stops mid-campaign already ends with a self-contained next-session prompt, posted on
the issue and repeated in the final report. In a remote container the user then had to copy that
block into a new session by hand. `mutsu-ticket-flow` ("Stopping mid-campaign") now adds a step: where
the `claude-code-remote` `create_session` tool is available, the agent asks whether to start the next
session and, on yes, passes the prompt to `create_session` and reports the new session's ID. The issue
comment and the fenced block stay as the durable record and as the handoff on the local dev box.
