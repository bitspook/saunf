use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub struct DevServerConfig {
    pub port: i32,
    pub host: String,
    pub template_dir: String,
    pub scss_dir: String,
    pub static_dir: String,
}

impl Default for DevServerConfig {
    fn default() -> Self {
        Self {
            port: 8000,
            host: "127.0.0.1".to_string(),
            template_dir: "src/web/templates".to_string(),
            scss_dir: "src/web/scss".to_string(),
            static_dir: "src/web/static".to_string(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct WebConfig {
    pub dev_server: DevServerConfig,
    pub base_url: String,
}

impl Default for WebConfig {
    fn default() -> Self {
        Self {
            dev_server: DevServerConfig::default(),
            base_url: "/".to_string()
        }
    }
}
