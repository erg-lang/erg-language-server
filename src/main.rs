mod server;
mod message;
mod hir_visitor;

fn main() {
    let mut server = server::ErgLanguageServer::new();
    server.run().unwrap();
}
