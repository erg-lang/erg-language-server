mod server;
mod message;
mod hir_visitor;

use erg_common::config::ErgConfig;

fn main() {
    let cfg = ErgConfig::default();
    let mut server = server::ErgLanguageServer::new(cfg);
    server.run().unwrap();
}
