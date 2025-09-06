# Makefile for SyncOptions LMS Plugin

# Configuration
PLUGIN_NAME = SyncOptions
VERSION = 2.3.0
PYTHON = python3

# Default target
all: build

# Build the plugin
build:
	@echo "Building $(PLUGIN_NAME) v$(VERSION)..."
	$(PYTHON) build.py

# Clean build artifacts
clean:
	rm -rf build/
	rm -f docs/SyncOptions/$(PLUGIN_NAME)-$(VERSION).zip

# Test the build (basic validation)
test: build
	@echo "Testing plugin structure..."
	@if [ -f "docs/SyncOptions/$(PLUGIN_NAME)-$(VERSION).zip" ]; then \
		echo "✓ ZIP file created successfully"; \
		unzip -t "docs/SyncOptions/$(PLUGIN_NAME)-$(VERSION).zip" > /dev/null && echo "✓ ZIP file is valid"; \
	else \
		echo "✗ ZIP file not found"; \
		exit 1; \
	fi

# Show current structure
structure:
	@echo "Current repository structure:"
	@find . -type f -not -path "./.git/*" -not -path "./build/*" | sort

# Help
help:
	@echo "Available targets:"
	@echo "  build     - Build the plugin ZIP and update repository"
	@echo "  clean     - Remove build artifacts"
	@echo "  test      - Build and test the plugin"
	@echo "  structure - Show repository structure"
	@echo "  help      - Show this help"

.PHONY: all build clean test structure help
