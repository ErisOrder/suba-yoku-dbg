use keyboard_query::{DeviceQuery, DeviceState};

pub struct KeyListener {
    callbacks: Vec<(u16, Box<dyn Fn()>)>
}

impl KeyListener {
    pub fn new() -> Self {
        Self { callbacks: vec![] }
    }

    pub fn register_cb<F: Fn() + 'static>(&mut self, key: u16, f: F) {
        self.callbacks.push((key, Box::new(f)));
    }

    pub fn listen(&self) -> ! {
        let device_state = DeviceState::new();
        let mut prev_keys = vec![];

        loop {
            let keys = device_state.get_keys();
            if keys != prev_keys {
                for key in keys.iter() {
                    let cb = self.callbacks.iter().find(|(k, _)| *k == *key);
                    if let Some(cb) = cb { cb.1() }
                }
            }
            prev_keys = keys;
        }
    }
}