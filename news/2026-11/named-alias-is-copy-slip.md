# `:f(:$format) is copy` owns its container when the argument is forwarded through a slip

A renamed named parameter marked `is copy` bound a forwarded read-only cell as is
when the call passed `:$format, |%args`, so the body's `$format = ...` died with
"Cannot assign to a readonly variable". The leaf now takes the cell's value.
WWW::Ollama's `ollama-client` (and so LLM::Functions `t/01`) hit this.
