select
  u.name,
  a.line_1
from
  users u
cross join addresses a;
