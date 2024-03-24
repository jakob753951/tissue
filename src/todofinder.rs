use std::str::FromStr;
use regex::RegexBuilder;

#[derive(Debug, PartialEq, Eq)]
pub enum IssueType {
    Bug,
    Feature,
    Improvement,
    Other,
}

impl FromStr for IssueType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "bug" => IssueType::Bug,
            "feature" => IssueType::Feature,
            "improvement" => IssueType::Improvement,
            _ => IssueType::Other,
        })
    }
}

pub struct ToDo {
    pub submission: Submission,
    pub description: String,
    pub assigned: Option<String>,
    pub issue_type: IssueType,
}

#[derive(Clone)]
pub struct Submission {
    pub line_number: usize,
    pub line: String,
    pub file_path: String,
    pub issuer: String,
    pub date: String,
}

pub fn is_to_do(line: &str) -> bool {
    line.to_lowercase().contains("todo")
}

// TODO bug: this function is not implemented; Assigned OthelloEngineer.
// TODO feature: implement this function; Assigned OthelloEngineer.
pub fn parse_submission(submission: Submission) -> Result<ToDo, String> {
    let re = RegexBuilder::new(r"
    \s*                                 # \s means whitespace and * is any amount of
    todo\s+(?<type>\w+):                # matches 'todo bug:'
    \s*
    (                                   # needed to make OR operator work properly
        (?<description_no_assigned>.+)
        |                               # OR operator
        (?<description>.+)
        ;
        \s*
        assigned                        # literal 'assigned'
        \s+                             # a non-zero amount of whitespace (must be space between assigned literal and the user)
        (?<assigned>[a-zA-Z0-9_-]+)     # the username should be alphanumeric plus underscores and dashes
        .*
    )
    ")
        .case_insensitive(true)
        .ignore_whitespace(true)
        .build().unwrap();
    let binding = submission.clone();
    let Some(caps) = re.captures(binding.line.as_str()) else {
        return Err("Pattern not found".to_string())
    };
    Ok(ToDo {
        submission,
        description: caps.name("description")
            .map(|m| m.as_str().to_string())
            .or(caps.name("description_no_assigned")
                    .map(|m| m.as_str().to_string()))
            .unwrap(),
        assigned: caps.name("assigned").map(|s| s.as_str().to_string()),
        issue_type: (&caps["type"]).parse()?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_to_do() {
        assert_eq!(is_to_do("TODO: implement this function"), true);
        assert_eq!(is_to_do("todo: implement this function"), true);
        assert_eq!(is_to_do("to do: implement this function"), false);
        assert_eq!(is_to_do("to do implement this function"), false);
    }

    #[test]
    fn test_parse_issue_type() {
        assert_eq!("bug".parse::<IssueType>(), Ok(IssueType::Bug));
        assert_eq!("feature".parse::<IssueType>(), Ok(IssueType::Feature));
        assert_eq!("improvement".parse::<IssueType>(), Ok(IssueType::Improvement));
        assert_eq!("other".parse::<IssueType>(), Ok(IssueType::Other));
    }

    #[test]
    fn test_parse_to_do_line() {
        let submission = Submission {
            line_number: 1,
            line: String::from("TODO bug: implement this function"),
            file_path: String::from("src/todofinder.rs"),
            issuer: String::from("OthelloEngineer"),
            date: String::from("2021-09-01"),
        };
        let to_do = parse_submission(submission).unwrap();
        assert_eq!(to_do.description, "implement this function");
        assert_eq!(to_do.assigned, None);
        assert_eq!(to_do.issue_type, IssueType::Bug);
    }

    #[test]
    fn test_parse_to_do_line_with_assigned() {
        let submission = Submission {
            line_number: 1,
            line: String::from("TODO feature: implement this function; assigned OthelloEngineer"),
            file_path: String::from("src/todofinder.rs"),
            issuer: String::from("OthelloEngineer"),
            date: String::from("2021-09-01"),
        };
        let to_do = parse_submission(submission).unwrap();
        assert_eq!(to_do.description, "implement this function");
        assert_eq!(to_do.assigned, Some(String::from("OthelloEngineer")));
        assert_eq!(to_do.issue_type, IssueType::Feature);
    }

    #[test]
    fn test_parse_to_do_line_with_assigned_and_semicolon() {
        let submission = Submission {
            line_number: 1,
            line: String::from("TODO feature: implement this function; now; assigned OthelloEngineer"),
            file_path: String::from("src/todofinder.rs"),
            issuer: String::from("OthelloEngineer"),
            date: String::from("2021-09-01"),
        };
        let to_do = parse_submission(submission).unwrap();
        assert_eq!(to_do.description, "implement this function; now");
        assert_eq!(to_do.assigned, Some(String::from("OthelloEngineer")));
        assert_eq!(to_do.issue_type, IssueType::Feature);
    }
}