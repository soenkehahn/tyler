mod layout;
mod util;
mod window;

use layout::{activate_property, layout};
use util::{cmd, AppError};
use window::get_current_desktop;

const SCREEN_FRAME_HEIGHT: i32 = 28;
const WINDOW_FRAME_WIDTH: i32 = 1;
const WINDOW_FRAME_HEIGHT: i32 = 37;

fn main() -> Result<(), AppError> {
    let desktop = get_current_desktop()?;
    let size = get_screen_size()?;
    for properties in layout(size, desktop) {
        activate_property(properties)?;
    }
    Ok(())
}

fn get_screen_size() -> Result<(i32, i32), AppError> {
    let output = cmd("xrandr", vec![])?;
    let foo: String = output.chars().filter(|x| x != &',').collect();
    let bar: Vec<&str> = foo.split(" ")
        .skip_while(|word| word != &"current")
        .collect();
    let width = bar.get(1).unwrap().parse()?;
    let height: i32 = bar.get(3).unwrap().parse()?;
    Ok((width, height - SCREEN_FRAME_HEIGHT))
}
