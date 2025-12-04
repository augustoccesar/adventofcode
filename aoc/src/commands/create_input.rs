use std::fs::{self, OpenOptions};

use crate::base_path;

/// Create a new empty example input for the year + day.
#[derive(clap::Args)]
pub struct Args {
    year: u16,
    day: u8,
}

impl Args {
    pub fn handle(&self) {
        let mut next_value = 0;

        for path in fs::read_dir(base_path().join("inputs"))
            .expect("should be able to read the inputs folder")
        {
            if let Ok(entry) = path
                && let Ok(file_type) = entry.file_type()
                && file_type.is_file()
            {
                let file_name = entry.file_name();
                let file_name = file_name
                    .to_str()
                    .expect("file name should be a valid utf-8 str");

                if file_name.starts_with(&format!("{}_{:0>2}", self.year, self.day))
                    && file_name.contains("example")
                {
                    let example_idx = file_name
                        .replace(&format!("{}_{:0>2}_example_", self.year, self.day), "")
                        .replace(".txt", "");

                    next_value = example_idx
                        .parse::<i32>()
                        .expect("suffix of the example should be a valid i32")
                        + 1;
                }
            }
        }

        let new_input_path = base_path().join(format!(
            "inputs/{}_{:0>2}_example_{next_value}.txt",
            self.year, self.day
        ));

        OpenOptions::new()
            .create(true)
            .write(true)
            .open(&new_input_path)
            .expect("should be able to create new input file");

        println!(
            "{}",
            new_input_path
                .to_str()
                .expect("new input file path should be a valid utf-8 str")
        );
    }
}
