use std::process::{Command, Stdio};

#[derive(Debug)]
pub struct AppError(pub String);

impl From<::std::io::Error> for AppError {
    fn from(error: ::std::io::Error) -> AppError {
        AppError(format!("{}", error))
    }
}

impl From<::std::num::ParseIntError> for AppError {
    fn from(error: ::std::num::ParseIntError) -> AppError {
        AppError(format!("{}", error))
    }
}

pub fn cmd(command: &str, args: Vec<&str>) -> Result<String, AppError> {
    let child = Command::new(command)
        .args(args)
        .stdout(Stdio::piped())
        .spawn()?;
    let output = child.wait_with_output()?;
    let result: String = (&*String::from_utf8_lossy(&output.stdout)).to_string();
    Ok(result)
}
