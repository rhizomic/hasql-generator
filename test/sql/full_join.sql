select
  u.name,
  a.line_1
from
  users u
full join addresses a on u.id = a.user_id

where u.id = $1;
