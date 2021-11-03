CREATE EXTENSION "uuid-ossp";


CREATE FUNCTION trigger_set_timestamp()
  RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE projects (
  name VARCHAR(200) PRIMARY KEY,
  display_name VARCHAR(400),
  description VARCHAR,
  created_at TIMESTAMP DEFAULT NOW() NOT NULL,
  updated_at TIMESTAMP DEFAULT NOW() NOT NULL
);
CREATE TRIGGER set_timestamp BEFORE UPDATE ON projects FOR EACH ROW EXECUTE
PROCEDURE trigger_set_timestamp();

CREATE TABLE tasks (
  id UUID PRIMARY KEY,
  title VARCHAR(400) NOT NULL,
  description VARCHAR,
  created_at TIMESTAMP DEFAULT NOW() NOT NULL,
  updated_at TIMESTAMP DEFAULT NOW() NOT NULL
);
