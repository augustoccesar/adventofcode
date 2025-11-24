use std::{
    fs::{self, OpenOptions},
    io::Write,
    process,
};

pub fn run(year: u16, day: u8) {
    process::Command::new("deno")
        .args([
            "run",
            "-A",
            "main.ts",
            "run",
            &format!("{}", year),
            &format!("{}", day),
        ])
        .current_dir(crate::base_path().join("typescript"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!("typescript/y{year}/d{day:0>2}.ts"));

    if !day_file_path.exists() {
        day_file_path
            .parent()
            .map(|path| fs::create_dir_all(path).unwrap());

        let mut day_file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(day_file_path)
            .unwrap();

        day_file
            .write_all(
                DAY_TEMPLATE
                    .replace("$padded_day", &format!("{day:0>2}"))
                    .replace("$day", &format!("{}", day))
                    .replace("$year", &format!("{}", year))
                    .as_bytes(),
            )
            .unwrap();

        let mut main_file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(crate::base_path().join("typescript/main.ts"))
            .unwrap();

        crate::file::insert_codegen(
            &mut main_file,
            "import_day",
            &format!("import \"./y{year}/d{day:0>2}.ts\";\n"),
        );

        process::Command::new("deno")
            .arg("fmt")
            .current_dir(crate::base_path().join("typescript"))
            .status()
            .unwrap();
    }
}

const DAY_TEMPLATE: &str = r#"import { RegisterDay, Day } from "../Day.ts";

@RegisterDay($year, $day)
export class Day$padded_day extends Day {
    override partOne(): string {
        return "-";
    }

    override partTwo(): string {
        return "-";
    }
}
"#;
