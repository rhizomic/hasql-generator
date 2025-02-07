insert into users
  ( email
  , name
  , favorite_hobby
  , created_at
  , updated_at
  )

select * from
  unnest
    ( $1
    , $2
    , $3
    , $4
    , $5
    )

returning id;
