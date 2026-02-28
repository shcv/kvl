.PHONY: test test-verbose install clean lint

# Run tests
test:
	python -m pytest

# Run tests with verbose output  
test-verbose:
	python -m pytest -v

# Install in development mode
install:
	pip install -e .

# Clean up build artifacts
clean:
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info/
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete

# Run linting
lint:
	-python -m flake8 kvl/ tests/
	-python -m black --check kvl/ tests/