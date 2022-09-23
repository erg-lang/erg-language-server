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

pub struct Server {
    _cfg: String,
    input: StdinLock<'static>,
    output: StdoutLock<'static>,
}

impl Server {
    pub fn new() -> Self {
        let input = stdin().lock();
        let output = stdout().lock();
        Self {
            _cfg: String::new(),
            input,
            output,
        }
    }

    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.init()?;
        loop {
            let msg = Value::from_str(&self.read_message()?)?;
            self.dispatch(msg)?;
        }
        // Ok(())
    }

    fn send<T: ?Sized + Serialize>(&mut self, message: &T) -> ELSResult<()> {
        let msg = serde_json::to_string(message)?;
        write!(self.output, "Content-Length: {}\r\n\r\n{}", msg.len(), msg)?;
        self.output.flush()?;
        Ok(())
    }

    fn send_log<S: Into<String>>(&mut self, msg: S) -> ELSResult<()> {
        self.send(&LogMessage::new(msg))
    }

    fn send_error<S: Into<String>>(&mut self, id: Option<i64>, code: i64, msg: S) -> ELSResult<()> {
        self.send(&ErrorMessage::new(id, json!({ "code": code, "message": msg.into() })))
    }

    fn send_invalid_req_error(&mut self) -> ELSResult<()> {
        self.send_error(None, -32601, "received an invalid request")
    }

    /// initialize ELS
    fn init(&mut self) -> ELSResult<()> {
        self.send_log("initializing ELS")
    }

    /// Copied and modified from RLS, https://github.com/rust-lang/rls/blob/master/rls/src/server/io.rs
    fn read_message(&mut self) -> Result<String, io::Error> {
        // Read in the "Content-Length: xx" part.
        let mut size: Option<usize> = None;
        loop {
            let mut buffer = String::new();
            self.input.read_line(&mut buffer)?;

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
        self.input.read_exact(&mut content)?;

        String::from_utf8(content).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    fn dispatch(&mut self, msg: Value) -> ELSResult<()> {
        match (msg.get("id").and_then(|i| i.as_i64()), msg.get("method").and_then(|m| m.as_str())) {
            (Some(id), Some(method)) => self.handle_method(&msg, id, method),
            (Some(_id), None) => {
                // ignore at this time
                Ok(())
            }
            (None, Some(notification)) => self.handle_notification(&msg, notification),
            _ => self.send_invalid_req_error(),
        }
    }

    fn handle_method(&mut self, _msg: &Value, id: i64, method: &str) -> ELSResult<()> {
        match method {
            "initialize" => {
                self.send_log("initialize")?;
                self.send(&json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": { "capabilities": { "textDocumentSync": 1 } }
                }))
            },
            other => {
                self.send_error(Some(id), -32600, format!("{other} is not supported"))
            }
        }
    }

    fn handle_notification(&mut self, msg: &Value, notification: &str) -> ELSResult<()> {
        match notification {
            "initialized" => self.send_log("successfully bound"),
            "textDocument/didOpen" => self.send_log(format!("Open: {}", msg["params"]["textDocument"]["uri"])),
            "textDocument/didChange" => self.send_log(format!("Change: {}", msg["params"]["textDocument"]["uri"])),
            _ => self.send_log(format!("received notification: {}", notification)),
        }
    }
}
