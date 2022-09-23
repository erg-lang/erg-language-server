use std::io::{StdoutLock, StdinLock, BufRead, Read};
// use std::path;
// use std::fs;
use std::io;
use std::io::{stdin, stdout, Write, /*BufRead as _*/};
use std::str::FromStr;

use serde::{Serialize};
use serde_json::{Value};
use serde_json::json;

use crate::message::{LogMessage, ErrorMessage};

type ELSResult<T> = Result<T, Box<dyn std::error::Error>>;

fn send<T: ?Sized + Serialize>(output: &mut StdoutLock, message: &T) -> ELSResult<()> {
    let msg = serde_json::to_string(message)?;
    write!(output, "Content-Length: {}\r\n\r\n{}", msg.len(), msg)?;
    output.flush()?;
    Ok(())
}

fn send_log<S: Into<String>>(output: &mut StdoutLock, msg: S) -> ELSResult<()> {
    send(output, &LogMessage::new(msg))
}

fn send_error<S: Into<String>>(output: &mut StdoutLock, id: Option<i64>, code: i64, msg: S) -> ELSResult<()> {
    send(output, &ErrorMessage::new(id, json!({ "code": code, "message": msg.into() })))
}

fn send_invalid_req_error(output: &mut StdoutLock) -> ELSResult<()> {
    send_error(output, None, -32601, "received an invalid request")
}

/// initialize ELS
fn init(output: &mut StdoutLock) -> ELSResult<()> {
    send_log(output, "initializing ELS")
}

fn read_message(input: &mut StdinLock) -> Result<String, io::Error> {
    // Read in the "Content-Length: xx" part.
    let mut size: Option<usize> = None;
    loop {
        let mut buffer = String::new();
        input.read_line(&mut buffer)?;

        // End of input.
        if buffer.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "EOF encountered in the middle of reading LSP headers",
            ));
        }

        // Header section is finished, break from the loop.
        if buffer == "\r\n" {
            break;
        }

        let res: Vec<&str> = buffer.split(' ').collect();

        // Make sure header is valid.
        if res.len() != 2 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Header '{}' is malformed", buffer),
            ));
        }
        let header_name = res[0].to_lowercase();
        let header_value = res[1].trim();

        match header_name.as_ref() {
            "content-length:" => {
                size = Some(header_value.parse::<usize>().map_err(|_e| {
                    io::Error::new(io::ErrorKind::InvalidData, "Couldn't read size")
                })?);
            }
            "content-type:" => {
                if header_value != "utf8" && header_value != "utf-8" {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Content type '{}' is invalid", header_value),
                    ));
                }
            }
            // Ignore unknown headers (specification doesn't say what to do in this case).
            _ => (),
        }
    }
    let size = match size {
        Some(size) => size,
        None => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Message is missing 'content-length' header",
            ));
        }
    };

    let mut content = vec![0; size];
    input.read_exact(&mut content)?;

    String::from_utf8(content).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

fn dispatch(output: &mut StdoutLock, msg: Value) -> ELSResult<()> {
    match (msg.get("id").and_then(|i| i.as_i64()), msg.get("method").and_then(|m| m.as_str())) {
        (Some(id), Some(method)) => handle_method(output, id, method),
        (Some(_id), None) => {
            // ignore at this time
            Ok(())
        }
        (None, Some(notification)) => handle_notification(output, notification),
        _ => send_invalid_req_error(output),
    }
}

fn handle_method(output: &mut StdoutLock, id: i64, method: &str) -> ELSResult<()> {
    match method {
        "initialize" => {
            send_log(output, "initialize")?;
            send(output, &json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": { "capabilities": { "textDocumentSync": 1 } }
            }))
        },
        other => {
            send_error(output, Some(id), -32600, format!("{other} is not supported"))
        }
    }
}

fn handle_notification(output: &mut StdoutLock, notification: &str) -> ELSResult<()> {
    match notification {
        "initialized" => send_log(output, "successfully bound"),
        _ => send_log(output, format!("received notification: {}", notification)),
    }
}

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut input = stdin().lock();
    let mut output = stdout().lock();
    init(&mut output)?;
    loop {
        let msg = Value::from_str(&read_message(&mut input)?)?;
        dispatch(&mut output, msg)?;
    }
    // Ok(())
}
