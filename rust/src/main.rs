//! KVL command-line tool.
//!
//! Standard commands shared across KVL implementations:
//!   parse <file>       Parse KVL to compacted JSON
//!   parse-raw <file>   Parse KVL to raw categorical JSON
//!   serialize          Serialize JSON from stdin to KVL on stdout
//!   merge <files...>   Merge KVL files, print compacted JSON

use std::io::Read;
use std::process::ExitCode;

use kvl::{auto_config_for_separator, DumpOptions, Value};

const USAGE: &str = "\
KVL (Key-Value Language) command line tool

Usage:
  kvl parse <file>            Parse a KVL file to compacted JSON
  kvl parse-raw <file>        Parse a KVL file to raw categorical JSON
  kvl serialize [-s SEP]      Serialize JSON from stdin to KVL on stdout
  kvl merge <file> <file>...  Merge KVL files using categorical merge
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("parse") => cmd_parse(&args[1..], false),
        Some("parse-raw") => cmd_parse(&args[1..], true),
        Some("serialize") => cmd_serialize(&args[1..]),
        Some("merge") => cmd_merge(&args[1..]),
        Some("--help") | Some("-h") | Some("help") => {
            print!("{USAGE}");
            return ExitCode::SUCCESS;
        }
        _ => {
            eprint!("{USAGE}");
            return ExitCode::from(2);
        }
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

fn read_file(path: &str) -> Result<String, String> {
    std::fs::read_to_string(path).map_err(|e| format!("Error: cannot read '{path}': {e}"))
}

fn print_json(value: &Value) -> Result<(), String> {
    let json = serde_json::to_string_pretty(value).map_err(|e| format!("Error: {e}"))?;
    println!("{json}");
    Ok(())
}

fn cmd_parse(args: &[String], raw: bool) -> Result<(), String> {
    let path = args.first().ok_or("Error: missing input file")?;
    let text = read_file(path)?;
    let value = if raw {
        kvl::parse(&text)
    } else {
        kvl::loads(&text)
    }
    .map_err(|e| format!("KVL Error: {e}"))?;
    print_json(&value)
}

fn cmd_serialize(args: &[String]) -> Result<(), String> {
    let mut separator: Option<&str> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-s" | "--separator" => {
                separator = Some(
                    args.get(i + 1)
                        .ok_or("Error: -s requires a separator argument")?,
                );
                i += 2;
            }
            other => return Err(format!("Error: unknown argument '{other}'")),
        }
    }

    let mut input = String::new();
    std::io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| format!("Error: cannot read stdin: {e}"))?;
    let data: Value =
        serde_json::from_str(&input).map_err(|e| format!("Invalid JSON input: {e}"))?;

    let (config, options) = match separator {
        Some(sep) => (
            auto_config_for_separator(sep),
            DumpOptions {
                include_header: true,
                ..DumpOptions::default()
            },
        ),
        None => (kvl::KvlConfig::default(), DumpOptions::default()),
    };
    let text = kvl::dumps_with(&data, &config, &options).map_err(|e| format!("KVL Error: {e}"))?;
    print!("{text}");
    Ok(())
}

fn cmd_merge(args: &[String]) -> Result<(), String> {
    if args.is_empty() {
        return Err("Error: merge requires at least one file".to_string());
    }
    let mut merged = Value::empty();
    for path in args {
        let text = read_file(path)?;
        let data = kvl::parse(&text).map_err(|e| format!("Merge failed for '{path}': {e}"))?;
        merged = kvl::merge(&merged, &data);
    }
    let compacted = kvl::compact(&merged, &kvl::KvlConfig::default());
    print_json(&compacted)
}
