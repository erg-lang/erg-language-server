use std::io::{self, BufReader};
use std::io::{stdin, stdout, Write, StdoutLock, StdinLock, BufRead, Read};
use std::str::FromStr;
use std::fs::File;

use serde::{Serialize, Deserialize};
use serde_json::{Value};
use serde_json::json;

use erg_common::color::{RED, YELLOW, RESET, GREEN};
use erg_common::config::{ErgConfig, Input};
use erg_common::traits::{Runnable, Stream, Locational};

use erg_compiler::ty::Type;
use erg_compiler::erg_parser::lex::Lexer;
use erg_compiler::erg_parser::ast::VarName;
use erg_compiler::erg_parser::token::{Token, TokenKind, TokenCategory};
use erg_compiler::AccessKind;
use erg_compiler::varinfo::VarInfo;
use erg_compiler::context::Context;
use erg_compiler::build_hir::HIRBuilder;
use erg_compiler::hir::{HIR};

use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, CompletionItem, CompletionItemKind, InitializeResult, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, CompletionOptions, PublishDiagnosticsParams, Url, OneOf, GotoDefinitionParams, GotoDefinitionResponse,
    HoverProviderCapability, HoverParams, HoverContents, MarkedString, ClientCapabilities, CompletionParams,
};

use crate::message::{LogMessage, ErrorMessage};

type ELSResult<T> = Result<T, Box<dyn std::error::Error>>;

pub struct Server {
    client_capas: ClientCapabilities,
    context: Option<Context>,
    hir: Option<HIR>, // TODO: should be ModuleCache
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
            hir: None,
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

    fn send_diagnostics(&mut self, uri: Url, diagnostics: Vec<Diagnostic>) -> ELSResult<()> {
        let params = PublishDiagnosticsParams::new(uri, diagnostics, None);
        if self.client_capas.text_document.as_ref()
            .map(|doc| doc.publish_diagnostics.is_some())
            .unwrap_or(false)
        {
            self.send(&json!({
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": params,
            }))?;
        } else {
            self.send_log("the client does not support diagnostics")?;
        }
        Ok(())
    }

    #[allow(clippy::field_reassign_with_default)]
    fn init(&mut self, msg: &Value, id: i64) -> ELSResult<()> {
        self.send_log("initializing ELS")?;
        #[allow(clippy::collapsible_if)]
        if msg.get("params").is_some() && msg["params"].get("capabilities").is_some() {
            self.client_capas = ClientCapabilities::deserialize(&msg["params"]["capabilities"])?;
        }
        // self.send_log(format!("set client capabilities: {:?}", self.client_capas))?;
        let mut result = InitializeResult::default();
        result.capabilities = ServerCapabilities::default();
        result.capabilities.text_document_sync = Some(TextDocumentSyncCapability::from(TextDocumentSyncKind::FULL));
        let mut comp_options = CompletionOptions::default();
        comp_options.trigger_characters = Some(vec![".".to_string(), ":".to_string()]);
        result.capabilities.completion_provider = Some(comp_options);
        result.capabilities.definition_provider = Some(OneOf::Left(true));
        result.capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));
        self.send(&json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": result,
        }))
    }

    fn exit(&mut self) -> ELSResult<()> {
        self.send_log("exiting ELS")?;
        std::process::exit(0);
    }

    fn shutdown(&mut self, id: i64) -> ELSResult<()> {
        self.send_log("shutting down ELS")?;
        self.send(&json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": json!(null),
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
            (Some(id), Some(method)) => self.handle_request(&msg, id, method),
            (Some(_id), None) => {
                // ignore at this time
                Ok(())
            }
            (None, Some(notification)) => self.handle_notification(&msg, notification),
            _ => self.send_invalid_req_error(),
        }
    }

    fn handle_request(&mut self, msg: &Value, id: i64, method: &str) -> ELSResult<()> {
        match method {
            "initialize" => self.init(msg, id),
            "shutdown" => self.shutdown(id),
            "textDocument/completion" => self.show_completion(msg),
            "textDocument/definition" => self.show_definition(msg),
            "textDocument/hover" => self.show_hover(msg),
            other => {
                self.send_error(Some(id), -32600, format!("{other} is not supported"))
            }
        }
    }

    fn handle_notification(&mut self, msg: &Value, method: &str) -> ELSResult<()> {
        match method {
            "initialized" => self.send_log("successfully bound"),
            "exit" => self.exit(),
            "textDocument/didOpen" => {
                let uri = Url::parse(msg["params"]["textDocument"]["uri"].as_str().unwrap())?;
                self.send_log(format!("{method}: {uri}"))?;
                self.check_file(
                    uri,
                    msg["params"]["textDocument"]["text"].as_str().unwrap(),
                )
            },
            "textDocument/didSave" => {
                let uri = Url::parse(msg["params"]["textDocument"]["uri"].as_str().unwrap())?;
                self.send_log(format!("{method}: {uri}"))?;
                let path = uri.to_file_path().unwrap();
                let mut code = String::new();
                File::open(&path)?.read_to_string(&mut code)?;
                self.check_file(uri, &code)
            },
            // "textDocument/didChange"
            _ => self.send_log(format!("received notification: {}", method)),
        }
    }

    fn remove_escape(msg: &str) -> String {
        msg.replace(RED, "").replace(YELLOW, "").replace(GREEN, "").replace(RESET, "")
    }

    fn check_file<S: Into<String>>(&mut self, uri: Url, code: S) -> ELSResult<()> {
        self.send_log(format!("checking {uri}"))?;
        let path = uri.to_file_path().unwrap();
        let mode = if path.to_string_lossy().ends_with(".d.er") { "declare" } else { "exec" };
        // don't use ErgConfig::with_path (cause module is main)
        let cfg = ErgConfig {
            input: Input::File(path),
            ..ErgConfig::default()
        };
        let mut hir_builder = HIRBuilder::new(cfg);
        match hir_builder.build(code.into(), mode) {
            Err(artifact) => {
                self.hir = artifact.hir;
                self.send_log(format!("found errors: {}", artifact.errors.len()))?;
                let mut uri_and_diags: Vec<(Url, Vec<Diagnostic>)> = vec![];
                for err in artifact.errors.into_iter() {
                    let uri = if let Input::File(path) = err.input {
                        Url::from_file_path(path).unwrap()
                    } else {
                        uri.clone()
                    };
                    let message = Self::remove_escape(&err.core.desc);
                    let start = Position::new(err.core.loc.ln_begin().unwrap_or(1) as u32 - 1, err.core.loc.col_begin().unwrap_or(0) as u32);
                    let end = Position::new(err.core.loc.ln_end().unwrap_or(1) as u32 - 1, err.core.loc.col_end().unwrap_or(0) as u32);
                    let err_code = err.core.kind as u8;
                    let severity = if (60..=100).contains(&err_code) || (180..=200).contains(&err_code) {
                        DiagnosticSeverity::WARNING
                    } else {
                        DiagnosticSeverity::ERROR
                    };
                    let diag = Diagnostic::new(Range::new(start, end), Some(severity), None, None, message, None, None);
                    if let Some((_, diags)) = uri_and_diags.iter_mut().find(|x| x.0 == uri) {
                        diags.push(diag);
                    } else {
                        uri_and_diags.push((uri, vec![diag]));
                    }
                }
                for (uri, diags) in uri_and_diags.into_iter() {
                    self.send_log(format!("{uri}, errs: {}", diags.len()))?;
                    self.send_diagnostics(uri, diags)?;
                }
            }
            Ok(artifact) => {
                self.hir = Some(artifact.hir);
                self.send_log(format!("checking {uri} passed"))?;
                self.send_diagnostics(uri, vec![])?;
            }
        }
        self.context = Some(hir_builder.pop_mod_ctx());
        Ok(())
    }

    fn show_completion(&mut self, msg: &Value) -> ELSResult<()> {
        self.send_log(format!("completion requested: {msg}"))?;
        let params = CompletionParams::deserialize(&msg["params"])?;
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let trigger = params.context.as_ref().unwrap().trigger_character.as_ref().map(|s| &s[..]);
        let acc = match trigger {
            Some(".") => AccessKind::Attr,
            Some(":") => AccessKind::Attr, // or type ascription
            _ => AccessKind::Name,
        };
        self.send_log(format!("AccessKind: {acc:?}"))?;
        let mut result = vec![];
        let context =
            if acc.is_local() { self.context.as_ref().unwrap() }
            else if let Some(ctx) = self.get_receiver_ctx(uri, pos)? {
                ctx
            } else {
                self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))?;
                return Ok(());
            };
        for (name, vi) in context.dir().into_iter() {
            if &context.name[..] != "<module>" && vi.vis.is_private() {
                continue;
            }
            let mut item = CompletionItem::new_simple(name.to_string(), vi.t.to_string());
            item.kind = match &vi.t {
                Type::Subr(subr) if subr.self_t().is_some() => Some(CompletionItemKind::METHOD),
                Type::Quantified(quant) if quant.self_t().is_some() => Some(CompletionItemKind::METHOD),
                Type::Subr(_) | Type::Quantified(_) => Some(CompletionItemKind::FUNCTION),
                Type::ClassType => Some(CompletionItemKind::CLASS),
                Type::TraitType => Some(CompletionItemKind::INTERFACE),
                t if &t.qual_name()[..] == "Module" || &t.qual_name()[..] == "GenericModule" => Some(CompletionItemKind::MODULE),
                _ if vi.muty.is_const() => Some(CompletionItemKind::CONSTANT),
                _ => Some(CompletionItemKind::VARIABLE),
            };
            result.push(item);
        }
        self.send_log(format!("completion items: {}", result.len()))?;
        self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))
    }

    fn show_definition(&mut self, msg: &Value) -> ELSResult<()> {
        self.send_log(format!("definition requested: {msg}"))?;
        let params = GotoDefinitionParams::deserialize(&msg["params"])?;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let result = if let Some(token) = Self::get_token(uri.clone(), pos)? {
            let prev = self.get_token_relatively(uri.clone(), pos, -1)?;
            // TODO: check attribute
            if prev.map(|t| t.is(TokenKind::Dot) || t.is(TokenKind::DblColon)).unwrap_or(false) {
                self.send_log("attribute")?;
                GotoDefinitionResponse::Array(vec![])
            } else if let Some((name, _vi)) = self.get_definition(&token)? {
                match Self::loc_to_range(name.loc()) {
                    Some(range) => {
                        self.send_log("found")?;
                        GotoDefinitionResponse::Array(vec![lsp_types::Location::new(uri, range)])
                    }
                    None => {
                        self.send_log("not found (maybe builtin)")?;
                        GotoDefinitionResponse::Array(vec![])
                    }
                }
            } else {
                GotoDefinitionResponse::Array(vec![])
            }
        } else {
            self.send_log("lex error occurred")?;
            GotoDefinitionResponse::Array(vec![])
        };
        self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))
    }

    fn get_definition(&mut self, token: &Token) -> ELSResult<Option<(VarName, VarInfo)>> {
        if !token.category_is(TokenCategory::Symbol) {
            self.send_log(format!("not symbol: {token}"))?;
            Ok(None)
        } else if let Ok((name, vi)) = self.context.as_ref().unwrap().get_var_info(token.inspect()) {
            Ok(Some((name.clone(), vi.clone())))
        } else {
            self.send_log("not found")?;
            Ok(None)
        }
    }

    fn show_hover(&mut self, msg: &Value) -> ELSResult<()> {
        self.send_log(format!("hover requested : {msg}"))?;
        let params = HoverParams::deserialize(&msg["params"])?;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let mut contents = vec![];
        match Self::get_token(uri.clone(), pos)?.map(|tok| self.get_definition(&tok)).transpose()? {
            Some(Some((name, vi))) => {
                if let Some(line) = name.ln_begin() {
                    let path = uri.to_file_path().unwrap();
                    let code_block = BufReader::new(File::open(&path)?).lines().nth(line - 1).unwrap()?;
                    let definition = MarkedString::from_language_code("erg".into(), code_block);
                    contents.push(definition);
                }
                let typ = MarkedString::from_language_code("erg".into(), format!("{name}: {}", vi.t));
                contents.push(typ);
            }
            // not found or not symbol, etc.
            Some(None) => {}
            // lex error, etc.
            None => {
                self.send_log("lex error")?;
            }
        }
        let result = json!({ "contents": HoverContents::Array(contents) });
        self.send(&json!({ "jsonrpc": "2.0", "id": msg["id"].as_i64().unwrap(), "result": result }))
    }

    fn loc_to_range(loc: erg_common::error::Location) -> Option<Range> {
        let start = Position::new(loc.ln_begin()? as u32 - 1, loc.col_begin()? as u32);
        let end = Position::new(loc.ln_end()? as u32 - 1, loc.col_end()? as u32);
        Some(Range::new(start, end))
    }

    fn pos_in_loc<L: Locational>(loc: &L, pos: Position) -> bool {
        (loc.ln_begin().unwrap()..=loc.ln_end().unwrap()).contains(&(pos.line as usize + 1))
        && (loc.col_begin().unwrap()..=loc.col_end().unwrap()).contains(&(pos.character as usize))
    }

    fn get_token(uri: Url, pos: Position) -> ELSResult<Option<Token>> {
        // FIXME: detect change
        let mut timeout = 300;
        let path = uri.to_file_path().unwrap();
        loop {
            let mut code = String::new();
            File::open(&path)?.read_to_string(&mut code)?;
            if let Ok(tokens) = Lexer::from_str(code).lex() {
                let mut token = None;
                for tok in tokens.into_iter() {
                    if Self::pos_in_loc(&tok, pos) {
                        token = Some(tok);
                        break;
                    }
                }
                if token.is_some() {
                    return Ok(token);
                }
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
            timeout -= 1;
            if timeout == 0 {
                return Ok(None);
            }
        }
    }

    /// plus_minus: 0 => same as get_token
    fn get_token_relatively(&mut self, uri: Url, pos: Position, plus_minus: isize) -> ELSResult<Option<Token>> {
        // FIXME: detect change
        let mut timeout = 300;
        let path = uri.to_file_path().unwrap();
        loop {
            let mut code = String::new();
            File::open(&path)?.read_to_string(&mut code)?;
            if let Ok(tokens) = Lexer::from_str(code).lex() {
                let mut found_index = None;
                for (i, tok) in tokens.iter().enumerate() {
                    if Self::pos_in_loc(tok, pos) {
                        found_index = Some(i);
                        break;
                    }
                }
                if let Some(idx) = found_index {
                    if let Some(token) = tokens.into_iter().nth((idx as isize + plus_minus) as usize) {
                        if !token.is(TokenKind::Newline) {
                            return Ok(Some(token));
                        }
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
            timeout -= 1;
            if timeout == 0 {
                return Ok(None);
            }
        }
    }

    fn get_receiver_ctx(&mut self, uri: Url, attr_marker_pos: Position) -> ELSResult<Option<&Context>> {
        let maybe_token = self.get_token_relatively(uri, attr_marker_pos, -1)?;
        if let Some(token) = maybe_token {
                if token.is(TokenKind::Symbol) {
                let var_name = token.inspect();
                self.send_log(format!("name: {var_name}"))?;
                let ctx = self.context.as_ref().unwrap().get_receiver_ctx(var_name);
                Ok(ctx)
            } else {
                self.send_log(format!("not name: {token}"))?;
                Ok(None)
            }
        } else {
            self.send_log("token not found")?;
            Ok(None)
        }
    }
}
