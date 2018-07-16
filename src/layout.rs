use util::AppError;
use window::{set_window_position, set_window_size, Desktop, Window};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct WindowWithProperties {
    window_id: Window,
    position: (i32, i32),
    size: (i32, i32),
}

pub fn activate_property(
    WindowWithProperties {
        window_id,
        size,
        position,
    }: WindowWithProperties,
) -> Result<(), AppError> {
    set_window_size(window_id, size)?;
    set_window_position(window_id, position)
}

pub fn layout((width, height): (i32, i32), desktop: Desktop) -> Vec<WindowWithProperties> {
    match desktop.windows.split_first() {
        None => vec![],
        Some((active, inactives)) => {
            let active_width = (width as f32 * 0.7) as i32;
            let mut result = vec![WindowWithProperties {
                window_id: *active,
                position: (0, 0),
                size: (active_width, height),
            }];
            let inactive_height = (height as f32 / inactives.len() as f32) as i32;
            for (i, inactive) in inactives.into_iter().enumerate() {
                result.push(WindowWithProperties {
                    window_id: *inactive,
                    position: (active_width, i as i32 * inactive_height),
                    size: (width - active_width, inactive_height),
                });
            }
            result
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn does_not_choke_on_empty_desktops() {
        assert_eq!(
            layout(
                (1000, 500),
                Desktop {
                    desktop: 0,
                    windows: vec![],
                }
            ),
            vec![]
        );
    }

    mod one_active_window {
        use super::*;

        fn first_window() -> WindowWithProperties {
            let foo = layout(
                (1000, 500),
                Desktop {
                    desktop: 0,
                    windows: vec![Window(23)],
                },
            );
            *foo.get(0).unwrap()
        }

        #[test]
        fn positions_the_active_window_at_0_0() {
            assert_eq!(first_window().window_id, Window(23));
            assert_eq!(first_window().position, (0, 0));
        }

        #[test]
        fn gives_the_active_window_70_percent_of_the_screen_width() {
            assert_eq!(first_window().size.0, 700);
        }

        #[test]
        fn gives_the_active_window_100_percent_of_the_screen_height() {
            assert_eq!(first_window().size.1, 500);
        }
    }

    mod inactive_windows {
        use super::*;

        fn inactive_windows() -> Vec<WindowWithProperties> {
            layout(
                (1000, 500),
                Desktop {
                    desktop: 0,
                    windows: vec![Window(23), Window(42), Window(51)],
                },
            ).into_iter()
                .skip(1)
                .collect()
        }

        #[test]
        fn positions_inactive_windows_right_of_the_active_window() {
            let xs: Vec<i32> = inactive_windows()
                .into_iter()
                .map(|x| x.position.0)
                .collect();
            assert_eq!(xs, vec![700, 700]);
        }

        #[test]
        fn positions_inactive_windows_from_top_to_bottom() {
            let ids: Vec<Window> = inactive_windows()
                .into_iter()
                .map(|x| x.window_id)
                .collect();
            assert_eq!(ids, vec![Window(42), Window(51)]);
            let ys: Vec<i32> = inactive_windows()
                .into_iter()
                .map(|x| x.position.1)
                .collect();
            assert_eq!(ys, vec![0, 250]);
        }

        #[test]
        fn sets_the_inactive_window_width_to_30_percent_of_the_screen_width() {
            let widths: Vec<i32> = inactive_windows().into_iter().map(|x| x.size.0).collect();
            assert_eq!(widths, vec![300, 300]);
        }

        #[test]
        fn sets_the_inactive_window_height_to_fill_the_screen_height() {
            let heights: Vec<i32> = inactive_windows().into_iter().map(|x| x.size.1).collect();
            assert_eq!(heights, vec![250, 250]);
        }
    }
}
