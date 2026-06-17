# ============================================================================
# midilib — build system
#
# Pure Erlang/OTP library (rebar3). This Makefile owns the shared variables and
# the aggregate targets, and includes one language module:
#
#   mk/erlang.mk  — the Erlang / BEAM side (targets suffixed -erl), help-erl
#
# Run everything through this Makefile, not the module directly.
#
#   make                      # help
#   make build                # compile
#   make test                 # eunit + ct
#   make check                # lint + test (+ coverage)
#   make coverage COVERAGE_MIN=80   # coverage with an enforced floor
# ============================================================================

# --- ANSI colors (shared) ---------------------------------------------------
BLUE   := \033[1;34m
GREEN  := \033[1;32m
YELLOW := \033[1;33m
RED    := \033[1;31m
CYAN   := \033[1;36m
RESET  := \033[0m

# --- Identity (shared by the included module) -------------------------------
PROJECT_NAME := midilib
APP_VERSION  := $(shell grep vsn src/$(PROJECT_NAME).app.src 2>/dev/null | cut -d'"' -f2)
GIT_COMMIT   := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
GIT_BRANCH   := $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
BUILD_TIME   := $(shell date -u '+%Y-%m-%dT%H:%M:%SZ')
OTP_VERSION  := $(shell erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' 2>/dev/null || echo "not installed")
REBAR        := rebar3

# Coverage floor — Makefile-owned and overridable (CDC miss M4). Starts at 0
# because only midibin is tested today; later arcs raise this default as their
# tests land. Override per-invocation:  make coverage COVERAGE_MIN=80
COVERAGE_MIN ?= 0

.DEFAULT_GOAL := help

# --- Language module --------------------------------------------------------
include mk/erlang.mk

# ============================================================================
# Help
# ============================================================================
.PHONY: help
help: help-general help-erl

.PHONY: help-general
help-general:
	@echo ""
	@echo "$(CYAN)╔══════════════════════════════════════════════════════════╗$(RESET)"
	@echo "$(CYAN)║$(RESET) $(BLUE)$(PROJECT_NAME) v$(APP_VERSION) — Erlang MIDI library build system$(RESET)         $(CYAN)║$(RESET)"
	@echo "$(CYAN)╚══════════════════════════════════════════════════════════╝$(RESET)"
	@echo ""
	@echo "$(GREEN)General:$(RESET)"
	@echo "  $(YELLOW)make build$(RESET)            - Compile the app"
	@echo "  $(YELLOW)make test$(RESET)             - Run eunit + Common Test"
	@echo "  $(YELLOW)make lint$(RESET)             - Run xref"
	@echo "  $(YELLOW)make format$(RESET)           - Format sources (erlfmt if available)"
	@echo "  $(YELLOW)make coverage$(RESET)         - Coverage (floor: COVERAGE_MIN=$(COVERAGE_MIN))"
	@echo "  $(YELLOW)make docs$(RESET)             - Generate documentation"
	@echo "  $(YELLOW)make check$(RESET)            - Full gate: lint + test"
	@echo "  $(YELLOW)make ci$(RESET)               - Alias for check"
	@echo "  $(YELLOW)make clean$(RESET)            - Clean build artifacts"
	@echo "  $(YELLOW)make info$(RESET)             - Show build information"
	@echo "  $(YELLOW)make check-tools$(RESET)      - Verify required tools"
	@echo ""

# ============================================================================
# Aggregate targets — fan out to the language module
# ============================================================================
.PHONY: build test lint format coverage docs check ci clean

build: compile-erl
	@echo "$(GREEN)✓ Built $(PROJECT_NAME)$(RESET)"

test: test-erl
	@echo ""
	@echo "$(GREEN)✓ All tests passed$(RESET)"
	@echo ""

lint: lint-erl
	@echo ""
	@echo "$(GREEN)✓ Lint passed$(RESET)"
	@echo ""

format: format-erl
	@echo "$(GREEN)✓ Formatted$(RESET)"

coverage: coverage-erl
	@echo "$(GREEN)✓ Coverage generated (floor $(COVERAGE_MIN)%)$(RESET)"

docs: docs-erl
	@echo "$(GREEN)✓ Docs generated$(RESET)"

check: lint test
	@echo ""
	@echo "$(GREEN)✓ All checks passed (lint + test)$(RESET)"
	@echo ""

ci: check
	@echo "$(GREEN)✓ CI gate passed$(RESET)"

clean: clean-erl
	@echo "$(GREEN)✓ Cleaned$(RESET)"

# ============================================================================
# Information
# ============================================================================
.PHONY: info
info:
	@echo ""
	@echo "$(CYAN)╔══════════════════════════════════════════════════════════╗$(RESET)"
	@echo "$(CYAN)║$(RESET)  $(BLUE)Build Information$(RESET)                                       $(CYAN)║$(RESET)"
	@echo "$(CYAN)╚══════════════════════════════════════════════════════════╝$(RESET)"
	@echo ""
	@echo "$(GREEN)Project:$(RESET)"
	@echo "  Name:           $(PROJECT_NAME) v$(APP_VERSION)"
	@echo "  Build Time:     $(BUILD_TIME)"
	@echo "  Workspace:      $$(pwd)"
	@echo ""
	@echo "$(GREEN)Git:$(RESET)"
	@echo "  Branch:         $(GIT_BRANCH)"
	@echo "  Commit:         $(GIT_COMMIT)"
	@echo ""
	@echo "$(GREEN)Tools:$(RESET)"
	@echo "  OTP:            $(OTP_VERSION)"
	@echo "  Rebar3:         $$($(REBAR) --version 2>/dev/null || echo 'not found')"
	@echo "  Coverage floor: $(COVERAGE_MIN)%"
	@echo ""

# ============================================================================
# Tool check
# ============================================================================
.PHONY: check-tools
check-tools:
	@echo "$(BLUE)Checking for required tools...$(RESET)"
	@echo "$(CYAN)Erlang / BEAM:$(RESET)"
	@command -v erl >/dev/null 2>&1 && echo "  $(GREEN)✓ erl found (OTP $(OTP_VERSION))$(RESET)" || echo "  $(RED)✗ erl not found$(RESET)"
	@command -v $(REBAR) >/dev/null 2>&1 && echo "  $(GREEN)✓ rebar3 found$(RESET)" || echo "  $(RED)✗ rebar3 not found$(RESET)"
	@echo "$(CYAN)Optional:$(RESET)"
	@command -v erlfmt >/dev/null 2>&1 && echo "  $(GREEN)✓ erlfmt found$(RESET)" || echo "  $(YELLOW)→ erlfmt not found (format-erl will skip)$(RESET)"
	@command -v git >/dev/null 2>&1 && echo "  $(GREEN)✓ git found$(RESET)" || echo "  $(RED)✗ git not found$(RESET)"
	@test -f src/$(PROJECT_NAME).app.src && echo "  $(GREEN)✓ $(PROJECT_NAME).app.src found$(RESET)" || echo "  $(RED)✗ src/$(PROJECT_NAME).app.src not found$(RESET)"
