select
  email,
  'foo' || name,
  updated_at
from users
where id = $1;
