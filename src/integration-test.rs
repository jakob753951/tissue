#[cfg(test)]
mod tests {
    use crate::{filereader, todofinder::Submission};

    use super::*;
    use std::fs;
    #[test]
    fn find_all_todos() {
        let files = match filereader::comments_from_file_in_project("examples") {
            Ok(files) => files,
            Err(e) => panic!("Error: {}", e),
        };
        let submissions : Vec<Submission> = Vec::new();
        for file in files {
            for line in file.lines {
                let submission = Submission {
                    file_path: file.file_path.clone(),
                    line_number: line.0,
                    line: line.1.clone(),
                };
                submissions.push(submission);
            }
        }
        
    }
}