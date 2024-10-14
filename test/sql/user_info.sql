select
  u.name,
  n.nickname,
  a.line_1,
  a.line_2,
  a.city
from
  users u
join addresses a on u.id = a.user_id
left join nicknames n on u.id = n.user_id

where
  u.name = $1
  and a.postal_code = $2;
