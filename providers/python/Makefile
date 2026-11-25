.PHONY: sync
sync:
	uv sync

.PHONY: format
format:
	uv run ruff format

.PHONY: lint
lint:
	uv run ruff check
	uv run ruff format --check

.PHONY: typecheck-basedpyright
typecheck-basedpyright:
	uv run basedpyright

.PHONY: typecheck-mypy
typecheck-mypy:
	uv run mypy

.PHONY: typecheck
typecheck: typecheck-basedpyright typecheck-mypy

.PHONY: test
test:
	uv run pytest

.PHONY: check
check: lint typecheck test

.PHONY: clean
clean:
	rm -rf .ruff_cache .mypy_cache .pytest_cache build dist
	find . -type d -name __pycache__ -exec rm -rf {} +
