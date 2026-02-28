#!/usr/bin/env python3
"""
Entry point for running kvl as a Python module.

This allows users to run: python -m kvl <command> <args>
"""

from kvl.cli import main

if __name__ == "__main__":
    main()