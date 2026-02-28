package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"github.com/shcv/kvl/go"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "usage: kvl <parse|parse-raw|serialize|merge> [args...]\n")
		os.Exit(1)
	}

	cmd := os.Args[1]
	switch cmd {
	case "parse":
		cmdParse()
	case "parse-raw":
		cmdParseRaw()
	case "serialize":
		cmdSerialize()
	case "merge":
		cmdMerge()
	default:
		fmt.Fprintf(os.Stderr, "unknown command: %s\n", cmd)
		os.Exit(1)
	}
}

func cmdParse() {
	if len(os.Args) < 3 {
		fmt.Fprintf(os.Stderr, "usage: kvl parse <file>\n")
		os.Exit(1)
	}

	path := os.Args[2]
	data, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error reading file: %v\n", err)
		os.Exit(1)
	}

	result, err := kvl.Loads(string(data))
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing KVL: %v\n", err)
		os.Exit(1)
	}

	output, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "error encoding JSON: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(string(output))
}

func cmdParseRaw() {
	if len(os.Args) < 3 {
		fmt.Fprintf(os.Stderr, "usage: kvl parse-raw <file>\n")
		os.Exit(1)
	}

	path := os.Args[2]
	data, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error reading file: %v\n", err)
		os.Exit(1)
	}

	result, err := kvl.ParseString(string(data))
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing KVL: %v\n", err)
		os.Exit(1)
	}

	output, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "error encoding JSON: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(string(output))
}

func cmdSerialize() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error reading stdin: %v\n", err)
		os.Exit(1)
	}

	var input map[string]any
	if err := json.Unmarshal(data, &input); err != nil {
		fmt.Fprintf(os.Stderr, "error parsing JSON: %v\n", err)
		os.Exit(1)
	}

	output, err := kvl.MarshalString(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error serializing KVL: %v\n", err)
		os.Exit(1)
	}

	fmt.Print(output)
}

func cmdMerge() {
	if len(os.Args) < 4 {
		fmt.Fprintf(os.Stderr, "usage: kvl merge <file1> <file2>\n")
		os.Exit(1)
	}

	path1 := os.Args[2]
	path2 := os.Args[3]

	data1, err := os.ReadFile(path1)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error reading file %s: %v\n", path1, err)
		os.Exit(1)
	}

	data2, err := os.ReadFile(path2)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error reading file %s: %v\n", path2, err)
		os.Exit(1)
	}

	parsed1, err := kvl.ParseString(string(data1))
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing %s: %v\n", path1, err)
		os.Exit(1)
	}

	parsed2, err := kvl.ParseString(string(data2))
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing %s: %v\n", path2, err)
		os.Exit(1)
	}

	merged := kvl.Merge(parsed1, parsed2)
	compacted := kvl.Compact(merged)

	output, err := json.MarshalIndent(compacted, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "error encoding JSON: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(string(output))
}
