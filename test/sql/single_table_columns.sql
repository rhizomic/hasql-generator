select
  id,
  email,
  name,
  created_at,
  updated_at
from users
where id = $1;
