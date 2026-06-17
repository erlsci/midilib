# ============================================================================
# mk/erlang.mk — the Erlang / BEAM side of midilib
#
# Included by ./Makefile. Expects the shared variables it defines (colors,
# PROJECT_NAME, REBAR, OTP_VERSION, COVERAGE_MIN, GIT_*). Do not run this
# module directly.
#
# All targets are suffixed `-erl` so they compose under the aggregate targets
# in ./Makefile.
# ============================================================================

# ----------------------------------------------------------------------------
# Help (Erlang section)
# ----------------------------------------------------------------------------
.PHONY: help-erl
help-erl:
	@echo "$(GREEN)Erlang / BEAM (rebar3):$(RESET)"
	@echo "  $(YELLOW)make compile-erl$(RESET)      - Compile the OTP app"
	@echo "  $(YELLOW)make test-erl$(RESET)         - Run eunit + Common Test"
	@echo "  $(YELLOW)make test-unit-erl$(RESET)    - Run eunit only"
	@echo "  $(YELLOW)make test-ct-erl$(RESET)      - Run Common Test only"
	@echo "  $(YELLOW)make test-proper-erl$(RESET)  - Run PropEr property tests"
	@echo "  $(YELLOW)make lint-erl$(RESET)         - Run xref"
	@echo "  $(YELLOW)make xref-erl$(RESET)         - Run xref only"
	@echo "  $(YELLOW)make dialyzer-erl$(RESET)     - Run dialyzer (deliberate; not in lint gate)"
	@echo "  $(YELLOW)make coverage-erl$(RESET)     - Coverage (proper + cover, floor COVERAGE_MIN)"
	@echo "  $(YELLOW)make docs-erl$(RESET)         - Generate documentation"
	@echo "  $(YELLOW)make format-erl$(RESET)       - Format sources (erlfmt if available)"
	@echo "  $(YELLOW)make shell-erl$(RESET)        - Start a rebar3 shell"
	@echo "  $(YELLOW)make clean-erl$(RESET)        - Clean BEAM build artifacts"
	@echo "  $(YELLOW)make distclean-erl$(RESET)    - Deep clean (remove _build)"
	@echo ""

# ----------------------------------------------------------------------------
# Building
# ----------------------------------------------------------------------------
.PHONY: compile-erl
compile-erl:
	@echo "$(BLUE)Compiling $(PROJECT_NAME)...$(RESET)"
	@$(REBAR) compile
	@echo "$(GREEN)✓ Compiled$(RESET)"

# ----------------------------------------------------------------------------
# Testing
# ----------------------------------------------------------------------------
.PHONY: test-erl test-unit-erl test-ct-erl test-proper-erl
test-erl:
	@echo "$(BLUE)Running tests (eunit + ct)...$(RESET)"
	@$(REBAR) do eunit, ct
	@echo "$(GREEN)✓ Tests passed$(RESET)"

test-unit-erl:
	@echo "$(BLUE)Running eunit...$(RESET)"
	@$(REBAR) eunit

test-ct-erl:
	@echo "$(BLUE)Running Common Test...$(RESET)"
	@$(REBAR) ct

test-proper-erl:
	@echo "$(BLUE)Running PropEr property tests...$(RESET)"
	@$(REBAR) as test proper

# ----------------------------------------------------------------------------
# Quality / linting
# ----------------------------------------------------------------------------
.PHONY: lint-erl xref-erl dialyzer-erl
lint-erl: xref-erl
	@echo "$(GREEN)✓ Lint passed$(RESET)"

xref-erl:
	@echo "$(BLUE)Running xref...$(RESET)"
	@$(REBAR) xref
	@echo "$(GREEN)✓ xref passed$(RESET)"

# Dialyzer is kept available but out of the default lint gate — run it
# deliberately.
dialyzer-erl:
	@echo "$(BLUE)Running dialyzer...$(RESET)"
	@$(REBAR) dialyzer

# ----------------------------------------------------------------------------
# Coverage — floor owned by the Makefile (COVERAGE_MIN), not the rebar alias.
# ----------------------------------------------------------------------------
.PHONY: coverage-erl
coverage-erl:
	@echo "$(BLUE)Generating coverage (floor $(COVERAGE_MIN)%)...$(RESET)"
	@$(REBAR) as test do proper -c, cover -v --min_coverage=$(COVERAGE_MIN)
	@echo "$(GREEN)✓ Coverage complete$(RESET)"

# ----------------------------------------------------------------------------
# Documentation
# ----------------------------------------------------------------------------
.PHONY: docs-erl
docs-erl:
	@echo "$(BLUE)Generating documentation...$(RESET)"
	@$(REBAR) ex_doc 2>/dev/null || $(REBAR) edoc 2>/dev/null || echo "$(YELLOW)→ no doc tool configured (ex_doc/edoc); skipping$(RESET)"
	@echo "$(GREEN)✓ Docs step complete$(RESET)"

# ----------------------------------------------------------------------------
# Formatting
# ----------------------------------------------------------------------------
.PHONY: format-erl
format-erl:
	@echo "$(BLUE)Formatting sources...$(RESET)"
	@command -v erlfmt >/dev/null 2>&1 && erlfmt -w 'src/*.erl' 'test/*.erl' 'include/*.hrl' \
		|| $(REBAR) fmt 2>/dev/null \
		|| echo "$(YELLOW)→ no erlfmt available (erlfmt / rebar3 fmt); skipping$(RESET)"

# ----------------------------------------------------------------------------
# REPL
# ----------------------------------------------------------------------------
.PHONY: shell-erl
shell-erl:
	@$(REBAR) shell

# ----------------------------------------------------------------------------
# Cleaning
# ----------------------------------------------------------------------------
.PHONY: clean-erl distclean-erl
clean-erl:
	@echo "$(BLUE)Cleaning BEAM artifacts...$(RESET)"
	@$(REBAR) clean
	@rm -rf logs erl_crash.dump doc ebin src/*.beam test/*.beam
	@echo "$(GREEN)✓ Clean complete$(RESET)"

distclean-erl: clean-erl
	@rm -rf _build
	@echo "$(GREEN)✓ Deep clean complete (_build removed)$(RESET)"
