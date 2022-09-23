use std::fs;
use std::io::{stdin, BufRead, Write};

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = fs::File::create("log.txt").unwrap();
    let input = stdin();
    for line in input.lock().lines() {
        let line = line?;
        // println!("{line}");
        writeln!(f, "{line}")?;
        // f.sync_data()?;
    }
    Ok(())
}
