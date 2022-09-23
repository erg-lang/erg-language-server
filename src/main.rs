mod server;
mod message;

fn main() {
    let mut server = server::Server::new();
    server.run().unwrap();
}
