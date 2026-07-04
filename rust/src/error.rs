//! Error and diagnostic types for KVL processing.

use std::fmt;

/// Errors produced while parsing or serializing KVL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KvlError {
    /// Error during parsing KVL text, with optional position info.
    Parse {
        message: String,
        line: Option<usize>,
        column: Option<usize>,
    },
    /// Error during serializing to KVL.
    Serialize(String),
    /// Generic error (e.g. raised through the serde layer).
    Message(String),
}

impl KvlError {
    pub fn parse(message: impl Into<String>, line: Option<usize>, column: Option<usize>) -> Self {
        KvlError::Parse {
            message: message.into(),
            line,
            column,
        }
    }

    pub fn serialize(message: impl Into<String>) -> Self {
        KvlError::Serialize(message.into())
    }
}

impl fmt::Display for KvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KvlError::Parse {
                message,
                line,
                column,
            } => {
                write!(f, "{message}")?;
                if let Some(line) = line {
                    write!(f, " at line {line}")?;
                    if let Some(column) = column {
                        write!(f, ", column {column}")?;
                    }
                }
                Ok(())
            }
            KvlError::Serialize(message) => write!(f, "{message}"),
            KvlError::Message(message) => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for KvlError {}

impl serde::de::Error for KvlError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        KvlError::Message(msg.to_string())
    }
}

impl serde::ser::Error for KvlError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        KvlError::Message(msg.to_string())
    }
}

/// Severity of a parse diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Warning,
    Error,
}

/// A warning or error diagnostic emitted during parsing (e.g. W001, W002).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub message: String,
    pub line: Option<usize>,
}
