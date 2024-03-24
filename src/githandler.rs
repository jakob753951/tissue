use std::process::Command;
use std::io::Error;
pub fn get_current_user() -> Result<String, Error> {
    let output = Command::new("git")
                         .args(["config", "user.name"]) 
                         .output();

    let output = match output {
        Ok(output) => output,
        Err(e) => {
            eprintln!("COMMAND Error: {}", e); // Use eprintln for errors
            return Err(e);
        },
    };

    if !output.status.success() {
        let err_msg = String::from_utf8_lossy(&output.stderr);
        eprintln!("Git command failed: {}", err_msg);
        return Err(Error::new(std::io::ErrorKind::Other, "Git command failed"));
    }

    let user = String::from_utf8(output.stdout).map_err(|e| {
        eprintln!("FORMATTING Error: {}", e);
        Error::new(std::io::ErrorKind::InvalidData, e)
    })?;

    Ok(user.trim().to_string()) // Trim the output to remove any newline characters

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_current_user() {
        let user = match get_current_user() {
            Ok(user) => user,
            Err(e) => panic!("Error: {}", e),
        };
        assert_eq!(user, "jolee18");
    }
}