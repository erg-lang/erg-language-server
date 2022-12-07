mod server;
mod message;
mod hir_visitor;

fn main() {
    let mut server = server::Server::new();
    server.run().unwrap();
}
