use rocket::Route;
use rocket_dyn_templates::Template;
use serde_json::json;

use crate::{web::WebResult, SaunfConfig};

#[get("/")]
async fn home() -> WebResult<Template> {
    let config = SaunfConfig::load()?.web;
    let base_url = config.base_url;

    let context = json!({ "base_url": base_url });

    Ok(Template::render("home", context))
}

pub fn routes() -> Vec<Route> {
    routes![home]
}
