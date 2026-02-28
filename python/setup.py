from setuptools import setup, find_packages
from pathlib import Path

# Read README for long description
this_directory = Path(__file__).parent
long_description = (this_directory / "README.org").read_text(encoding="utf-8")

setup(
    name="kvl",
    version="0.1.0",
    description="Key-Value Configuration Language implementation in Python",
    long_description=long_description,
    long_description_content_type="text/x-org",
    author="Samuel H. Christie V",
    author_email="shcv@sdf.org",
    url="https://github.com/shcv/kvl/tree/main/python",
    project_urls={
        "Bug Reports": "https://github.com/shcv/kvl/issues",
        "Source": "https://github.com/shcv/kvl/tree/main/python",
        "Documentation": "https://github.com/shcv/kvl/tree/main/python#readme",
        "Language Specification": "https://github.com/shcv/kvl/tree/main/spec",
    },
    packages=find_packages(),
    python_requires=">=3.7",
    keywords="configuration config kvl key-value language schema validation parsing serialization",
    entry_points={
        "console_scripts": [
            "kvl=kvl.cli:main",
        ],
    },
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Text Processing :: Markup",
        "Topic :: System :: Systems Administration",
        "License :: Public Domain",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Operating System :: OS Independent",
    ],
    license="CC0",
)
