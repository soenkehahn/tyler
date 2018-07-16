use util::{cmd, AppError};

#[derive(Debug)]
pub struct Desktop {
    pub desktop: i32,
    pub windows: Vec<Window>,
}

pub fn get_current_desktop() -> Result<Desktop, AppError> {
    let desktop: i32 = cmd("xdotool", vec!["get_desktop"])?.trim().parse()?;
    let windows = get_windows(desktop)?;
    Ok(Desktop { desktop, windows })
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Window(pub i32);

fn get_windows(desktop: i32) -> Result<Vec<Window>, AppError> {
    let output = cmd(
        "xdotool",
        vec!["search", "--desktop", &desktop.to_string(), "."],
    )?;
    let mut windows: Vec<Window> = vec![];
    for line in output.trim().split("\n") {
        windows.push(Window(line.parse()?));
    }
    windows.reverse();
    Ok(windows)
}

pub fn set_window_size(Window(id): Window, (width, height): (i32, i32)) -> Result<(), AppError> {
    cmd(
        "xdotool",
        vec![
            "windowsize",
            &id.to_string(),
            &width.to_string(),
            &height.to_string(),
        ],
    )?;
    Ok(())
}

pub fn set_window_position(Window(id): Window, (x, y): (i32, i32)) -> Result<(), AppError> {
    cmd(
        "xdotool",
        vec![
            "windowmove",
            &id.to_string(),
            &x.to_string(),
            &y.to_string(),
        ],
    )?;
    Ok(())
}
