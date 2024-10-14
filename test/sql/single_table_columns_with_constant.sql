select
  email,
  'foo',
  updated_at
from users
where id = $1;
