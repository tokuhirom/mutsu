# Browser runs can be stopped

The playground, snippets, REPL, and embeddable code element now run their WASM interpreters in Web Workers. Stop terminates a running worker so a non-terminating program cannot freeze the page. The next run starts a fresh interpreter. A WASM trap also discards its worker and instance before another run.
