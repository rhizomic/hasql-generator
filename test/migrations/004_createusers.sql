create table users (
  id uuid primary key unique default uuid_generate_v4(),
  email email not null,
  name varchar not null,
  created_at timestamptz not null,
  updated_at timestamptz not null
);
