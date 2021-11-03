#[macro_use]
extern crate diesel;

mod config;
pub use config::SaunfConfig;

pub mod db;
pub mod web;

#[macro_use]
extern crate rocket;
