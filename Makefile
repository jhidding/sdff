.PHONY: repl repl-sessions site

repl-session := uv run repl-session

repl:
	scheme --libdirs ./scheme

repl_session_dir := .entangled/repl-session
sessions := $(filter-out %.out.json, $(wildcard $(repl_session_dir)/*))
$(info Found REPL sessions: $(sessions))
sessions_out := $(sessions:%.json=%.out.json)

repl-sessions: $(sessions_out)

$(repl_session_dir)/%.out.json: $(repl_session_dir)/%.json
	$(repl-session) < $< > $@

site: $(sessions_out)
	uv run mkdocs build
