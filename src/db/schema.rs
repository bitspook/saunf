table! {
    projects (name) {
        name -> Varchar,
        display_name -> Nullable<Varchar>,
        description -> Nullable<Varchar>,
        created_at -> Timestamp,
        updated_at -> Timestamp,
    }
}

table! {
    tasks (id) {
        id -> Uuid,
        title -> Varchar,
        description -> Nullable<Varchar>,
        created_at -> Timestamp,
        updated_at -> Timestamp,
    }
}

allow_tables_to_appear_in_same_query!(
    projects,
    tasks,
);
