use anyhow::Context;
use crate::web::WebConfig;
use rocket::figment::{
    providers::{Env, Format, Serialized, Toml},
    Figment,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub struct SaunfConfig {
    pub database_url: String,
    pub web: WebConfig,
}

impl Default for SaunfConfig {
    fn default() -> Self {
        Self {
            database_url: "postgresql:///saunf?host=./postgres/run".to_string(),
            web: WebConfig::default(),
        }
    }
}

impl SaunfConfig {
    pub fn load() -> anyhow::Result<SaunfConfig> {
        let config: SaunfConfig = Figment::new()
            .merge(Serialized::defaults(SaunfConfig::default()))
            .merge(Toml::file("Saunf.toml"))
            .merge(Env::prefixed("SAUNF_").global())
            .extract()
            .with_context(|| "Invalid Saunf Configuration.")?;

        Ok(config)
    }
}
