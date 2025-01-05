select
  u.name,
  u.favorite_hobby,
  a.line_1,
  n.nickname
from
  users u
left join nicknames n on u.id = n.user_id
join addresses a on u.id = a.user_id

where u.id = $1;
