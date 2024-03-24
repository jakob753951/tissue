use std::{collections::HashMap, fs};
pub struct FileLines {
    pub file_path: String,
    pub lines: HashMap<usize, String>
}
fn get_all_files_in_directory(directory: &str) -> Result<Vec<String>, String> {
    let mut files = Vec::new();
    let paths = match fs::read_dir(directory) {
        Ok(paths) => paths,
        Err(e) => return Err(e.to_string()),
    };

    for path in paths {
        let path = path.map_err(|e| e.to_string())?.path();
        if path.is_file() {
            files.push(path.to_str().unwrap().to_string());
        } else if path.is_dir() {
            let mut sub_files = get_all_files_in_directory(path.to_str().unwrap())?;
            files.append(&mut sub_files);
        }
    }
    Ok(files)
}

fn read_comments_from_file(file_path: &str) -> Result<FileLines, String> {
    let file = match fs::read_to_string(file_path) {
        Ok(file) => file,
        Err(e) => return Err(e.to_string()),
    };
    let lines: HashMap<usize, String> = file
        .lines()
        .enumerate()
        .filter(|line| is_comment(line.1))
        .map(|line| (line.0+1, line.1.to_string()))
        .collect();
    Ok(FileLines {
        file_path: file_path.to_string(),
        lines,
    })
}

pub fn comments_from_file_in_project(directory: &str) -> Result<Vec<FileLines>, String> {
    let files = get_all_files_in_directory(directory)?;
    let mut file_lines = Vec::new();
    for file in files {
        let lines = read_comments_from_file(&file)?;
        file_lines.push(lines);
    }
    Ok(file_lines)
}

fn is_comment(line: &str) -> bool {
    line.trim().starts_with("//")
        || line.trim().starts_with("#")
        || line.trim().starts_with(";")
        || line.trim().starts_with("--")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_all_files_in_directory() {
        let files = match get_all_files_in_directory("examples") {
            Ok(files) => files,
            Err(e) => panic!("Error: {}", e),
        };
        assert_eq!(files.len(), 8);
    }

    #[test]
    fn test_read_lines_from_file() {
        let file = match read_comments_from_file("examples/example.rs") {
            Ok(lines) => lines,
            Err(e) => panic!("Error: {}", e),
        };
        assert_eq!(file.lines.len(), 2);
    }

    #[test]
    fn test_lines_with_file_from_dir() {
        let files = match comments_from_file_in_project("examples") {
            Ok(files) => files,
            Err(e) => panic!("Error: {}", e),
        };
        assert_eq!(files.len(), 8);
        assert!(files.iter()
            .map(|f| f.file_path.clone())
            .collect::<Vec<_>>()
            .contains(&"examples/example.py".to_string()));
        assert!(files.iter()
            .map(|f| f.lines.len())
            .collect::<Vec<_>>()
            .contains(&2));
    }
}
