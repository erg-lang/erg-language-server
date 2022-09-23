use std::io;
use std::io::{stdin, stdout, Write, StdoutLock, StdinLock, BufRead, Read};
use std::path::PathBuf;
use std::str::FromStr;
use std::fs::File;

use serde::{Serialize, Deserialize};
use serde_json::{Value};
use serde_json::json;

use erg_common::color::{RED, YELLOW, RESET};
use erg_common::config::{ErgConfig, Input};
use erg_common::traits::{Runnable, Stream};

use erg_type::Type;

use erg_compiler::AccessKind;
use erg_compiler::context::Context;
use erg_compiler::build_hir::HIRBuilder;

use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, CompletionItem, CompletionItemKind, InitializeResult, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, CompletionOptions,
};

use url::Url;

use crate::message::{LogMessage, ErrorMessage};

type ELSResult<T> = Result<T, Box<dyn std::error::Error>>;

fn get_path_from_uri(uri: &str) -> ELSResult<String> {
    let url = Url::parse(uri)?;
    let path = urlencoding::decode(url.path())?.to_string();
    Ok(path[1..].to_string())
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ClientCapabilities {
    pub publish_diagnostics: bool,
}

pub struct Server {
    client_capas: ClientCapabilities,
    context: Option<Context>,
    input: StdinLock<'static>,
    output: StdoutLock<'static>,
}

impl Server {
    pub fn new() -> Self {
        let input = stdin().lock();
        let output = stdout().lock();
        Self {
            client_capas: ClientCapabilities::default(),
            context: None,
            input,
            output,
        }
    }

    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
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

    fn send_diagnostics(&mut self, uri: &str, diagnostics: Vec<Diagnostic>) -> ELSResult<()> {
        if self.client_capas.publish_diagnostics {
            self.send(&json!({
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {
                    "uri": uri,
                    "diagnostics": diagnostics,
                }
            }))?;
        } else {
            self.send_log("client does not support diagnostics")?;
        }
        Ok(())
    }

    #[allow(clippy::field_reassign_with_default)]
    fn init(&mut self, msg: &Value, id: i64) -> ELSResult<()> {
        self.send_log("initializing ELS")?;
        #[allow(clippy::collapsible_if)]
        if msg.get("params").is_some() && msg["params"].get("capabilities").is_some() {
            if msg["params"]["capabilities"].get("textDocument").is_some() && msg["params"]["capabilities"]["textDocument"].get("publishDiagnostics").is_some() {
                // TODO: more detailed configuration
                self.client_capas.publish_diagnostics = true;
            }
            // and other capabilities
        }
        // self.send_log(format!("set client capabilities: {:?}", self.client_capas))?;
        let mut result = InitializeResult::default();
        result.capabilities = ServerCapabilities::default();
        result.capabilities.text_document_sync = Some(TextDocumentSyncCapability::from(TextDocumentSyncKind::FULL));
        let mut comp_options = CompletionOptions::default();
        comp_options.trigger_characters = Some(vec![".".to_string(), "::".to_string()]);
        result.capabilities.completion_provider = Some(comp_options);
        self.send(&json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": result,
        }))
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

    fn handle_method(&mut self, msg: &Value, id: i64, method: &str) -> ELSResult<()> {
        match method {
            "initialize" => self.init(msg, id),
            "textDocument/completion" => self.show_completion(msg),
            other => {
                self.send_error(Some(id), -32600, format!("{other} is not supported"))
            }
        }
    }

    fn handle_notification(&mut self, msg: &Value, notification: &str) -> ELSResult<()> {
        match notification {
            "initialized" => self.send_log("successfully bound"),
            "textDocument/didOpen" => {
                let uri = msg["params"]["textDocument"]["uri"].as_str().unwrap();
                let path = get_path_from_uri(uri)?;
                self.send_log(format!("{notification}: {uri}"))?;
                self.check_file(
                    uri,
                    &path,
                    msg["params"]["textDocument"]["text"].as_str().unwrap(),
                )
            },
            "textDocument/didSave" => {
                let uri = msg["params"]["textDocument"]["uri"].as_str().unwrap();
                let path = get_path_from_uri(uri)?;
                let mut code = String::new();
                self.send_log(format!("{notification}: {path}"))?;
                File::open(&path)?.read_to_string(&mut code)?;
                self.check_file(uri, &path, &code)
            },
            // "textDocument/didChange"
            _ => self.send_log(format!("received notification: {}", notification)),
        }
    }

    fn check_file<S: Into<String>>(&mut self, uri: &str, path: &str, code: S) -> ELSResult<()> {
        self.send_log(format!("checking {path}"))?;
        let cfg = ErgConfig {
            input: Input::File(PathBuf::from(path)),
            mode: "exec",
            opt_level: 1,
            dump_as_pyc: false,
            python_ver: None,
            py_server_timeout: 10,
            quiet_startup: false,
            module: "<module>",
            verbose: 2,
            ps1: ">>> ",
            ps2: "... ",
        };
        let mut hir_builder = HIRBuilder::new(cfg);
        match hir_builder.build(code.into(), "exec") {
            Err(errs) => {
                self.send_log(format!("found errors: {}", errs.len()))?;
                let diags = errs.into_iter().map(|err| {
                    let message = err.core.desc.to_string().replace(RED, "").replace(YELLOW, "").replace(RESET, "");
                    let start = Position::new(err.core.loc.ln_begin().unwrap() as u32 - 1, err.core.loc.col_begin().unwrap() as u32);
                    let end = Position::new(err.core.loc.ln_end().unwrap() as u32 - 1, err.core.loc.col_end().unwrap() as u32);
                    let err_code = err.core.kind as u8;
                    let severity = if (60..=100).contains(&err_code) || (180..=200).contains(&err_code) {
                        DiagnosticSeverity::WARNING
                    } else {
                        DiagnosticSeverity::ERROR
                    };
                    Diagnostic::new(Range::new(start, end), Some(severity), None, None, message, None, None)
                }).collect();
                self.send_diagnostics(uri, diags)?;
            }
            Ok(_) => {
                // self.hir = Some(hir);
                self.send_log(format!("checking {path} passed"))?;
                self.send_diagnostics(uri, vec![])?;
            }
        }
        self.context = Some(hir_builder.pop_ctx());
        Ok(())
    }

    fn show_completion(&mut self, msg: &Value) -> ELSResult<()> {
        self.send_log(format!("showing completion: {msg}"))?;
        let trigger = msg["params"]["context"]["triggerCharacter"].as_str();
        let acc = match trigger {
            Some(".")| Some("::") => AccessKind::Attr,
            _ => AccessKind::Name,
        };
        self.send_log(format!("AccessKind: {acc:?}"))?;
        let mut result = vec![];
        let context = if acc.is_local() { self.context.as_ref().unwrap() } else {
            // TODO: get context
            self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))?;
            return Ok(());
        };
        for (name, vi) in context.dir().into_iter() {
            let mut item = CompletionItem::new_simple(name.to_string(), vi.t.to_string());
            item.kind = match &vi.t {
                Type::Subr(_) => Some(CompletionItemKind::FUNCTION),
                Type::Module => Some(CompletionItemKind::MODULE),
                Type::Class | Type::Trait => Some(CompletionItemKind::CLASS),
                _ if vi.muty.is_const() => Some(CompletionItemKind::CONSTANT),
                _ => Some(CompletionItemKind::VARIABLE),
            };
            result.push(item);
        }
        // CompletionTriggerKind
        self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))
    }
}
