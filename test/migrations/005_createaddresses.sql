create table addresses (
  id uuid primary key unique default uuid_generate_v4(),
  user_id uuid references users not null,
  line_1 text not null,
  line_2 text,
  city text not null,
  postal_code text not null,
  country text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null
);
