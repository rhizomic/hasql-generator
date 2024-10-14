select
  u.name,
  a.line_1,
  n.nickname
from
  users u
join nicknames n on u.id = n.user_id
right join addresses a on u.id = a.user_id

where a.user_id = $1;
