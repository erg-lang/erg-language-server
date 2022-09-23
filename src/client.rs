use std::io::StdoutLock;
// use std::path;
// use std::fs;
use std::io::{stdin, stdout, Write, /*BufRead as _*/};

use serde::{Serialize};

use crate::message::{LogMessage};

fn send<T: ?Sized + Serialize>(output: &mut StdoutLock, message: &T) -> Result<(), Box<dyn std::error::Error>> {
    let msg = serde_json::to_string(message)?;
    write!(output, "Content-Length: {}\r\n\r\n{}", msg.len(), msg)?;
    output.flush()?;
    Ok(())
}

fn log<S: Into<String>>(output: &mut StdoutLock, msg: S) -> Result<(), Box<dyn std::error::Error>> {
    send(output, &LogMessage::new(msg))
}

fn init(output: &mut StdoutLock) -> Result<(), Box<dyn std::error::Error>> {
    log(output, "initialized")
}

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input = stdin();
    let mut output = stdout().lock();
    init(&mut output)?;
    let mut buf = String::new();
    loop {
        input.read_line(&mut buf)?;
        if buf.starts_with("Content-Length") || buf.is_empty() { continue; }
    }
    // Ok(())
}
