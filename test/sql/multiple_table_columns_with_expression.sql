select
  name,
  'foo' || name,
  a.line_1,
  a.line_2,
  a.country
from
  users u
join addresses a on u.id = a.user_id

where
      u.id = $1
  and a.country = $2;
