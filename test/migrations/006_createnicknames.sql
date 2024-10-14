create table nicknames (
  id uuid primary key unique default uuid_generate_v4(),
  user_id uuid references users not null,
  nickname text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null
);
