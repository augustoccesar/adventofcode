use std::{
    fs::File,
    io::{Read, Seek, Write},
};

pub fn insert_codegen(file: &mut File, key: &str, insert_content: &str) {
    let mut file_content = String::new();
    file.read_to_string(&mut file_content).unwrap();

    let idx = [format!("// CODEGEN:{key}"), format!("# CODEGEN:{key}")]
        .iter()
        .find_map(|pattern| file_content.find(pattern))
        .expect("CODEGEN directive not found");

    file_content.insert_str(idx, insert_content);

    file.rewind().unwrap();
    file.set_len(0).unwrap();
    file.write_all(file_content.as_bytes()).unwrap();

    file.rewind().unwrap();
}
