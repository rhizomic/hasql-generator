# hasql-generator

This library generates Haskell code for interfacing with Hasql. It abstracts
the tedium of having to manually write and maintain Hasql codecs.

## Motivation

1. [hasql](https://hackage.haskell.org/package/hasql) is a wonderful library,
   but writing encoders and decoders for every query quickly grows tiresome.
   For projects with more than a few queries, using the base package just isn't
   feasible.
2. While [hasql-th](https://hackage.haskell.org/package/hasql-th) eliminates
   the need to deal with codecs, users are still forced to annotate the types
   and nullability constraints in their queries. Because the code has no
   knowledge of the underlying schema, these annotations can lead to bugs if
   the schema changs.
3. Another downside to `hasql-th` is that the queries themselves need to be
   written in (Template) Haskell, which means that users lose the benefit of
   SQL syntax highlighting.

This library was written to solve all of the above pain points:

1. Codecs are now auto-generated.
2. The generated code takes the underlying schema into account. This includes
   both types _and_ nullability constraints.
3. All application queries can be written in standalone SQL files. No Template
   Haskell is required at all.

## Example

Given the following schema:

```sql
create table users (
  id uuid primary key unique default uuid_generate_v4(),
  name varchar not null
);

create table nicknames (
  id uuid primary key unique default uuid_generate_v4(),
  user_id uuid references users not null,
  nickname text not null
);

create table addresses (
  id uuid primary key unique default uuid_generate_v4(),
  user_id uuid references users not null,
  line_1 text not null,
  line_2 text,
  city text not null,
  postal_code text not null,
  country text not null
);
```

And then the following query:

```sql
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
```

This library will generate the following code:

```hs
module QueryToGetUserInfo
  (query)
where

import Contravariant.Extras (contrazip2)
import Control.Applicative ((<*>))
import Data.Bool (Bool (True))
import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement (Statement))
import Hasql.Transaction (Transaction, statement)

query ::
  UUID ->
  Text ->
  Transaction (Vector (Text, Maybe Text, Text, Maybe Text, Text))
query a1 a2 =
  statement
   params
   (Statement sql encoder decoder True)

  where
    sql :: ByteString
    sql = "select\n  u.name,\n  n.nickname,\n  a.line_1,\n  a.line_2,\n  a.city\nfrom\n  users u\njoin addresses a on u.id = a.user_id\nleft join nicknames n on u.id = n.user_id\n\nwhere\n  u.name = $1\n  and a.postal_code = $2;\n"

    params :: (UUID, Text)
    params = (a1, a2)

    encoder :: Encoders.Params (UUID, Text)
    encoder =
      contrazip2
        (Encoders.param $ Encoders.nonNullable Encoders.uuid)
        (Encoders.param $ Encoders.nonNullable Encoders.text)

    decoder :: Decoders.Result (Vector (Text, Maybe Text, Text, Maybe Text, Text))
    decoder =
      Decoders.rowVector $
        (,,,,)
          <$> Decoders.column (Decoders.nonNullable Decoders.text)
          <*> Decoders.column (Decoders.nullable Decoders.text)
          <*> Decoders.column (Decoders.nonNullable Decoders.text)
          <*> Decoders.column (Decoders.nullable Decoders.text)
          <*> Decoders.column (Decoders.nonNullable Decoders.text)
```

## Usage

In order for `hasql-generator` to have knowledge of your underlying schema, it
will need to be given a path to a dump of the schema. (The dump can be
generated with `pg_dump`, e.g.,
`pg_dump -U $DB_USER --no-owner --schema-only $DB_NAME > schema.sql`).

```hs
import Data.List.NonEmpty (fromList)
import Data.Map.Strict (empty)
import Hasql.Generator (generate)
import Hasql.Generator.Types (QueryConfig (..))

generateCodecs :: IO (Either (NonEmpty (Text, QueryConfig)) ())
generateCodecs = do
  let schemaFile = "path/to/schema.sql"
      queryConfigs = fromList
        [ QueryConfig
          { inputFile = "path/to/query_to_get_user_info.sql"
          , outputLocation = "path/to/QueryToGetUserInfo.hs"
          , moduleName = "QueryToGetUserInfo"
          , functionName = "query"
          }
        ]
      enumConfigs = empty

  generate migrationFiles queryConfigs enumConfigs
```

Handling PostgreSQL enums requires some additional setup. Given the following
schema:

```sql
create type hobby as enum (
  'Reading',
  'Writing',
  'PlayingSports'
);

create table users (
  id uuid primary key unique default uuid_generate_v4(),
  favorite_hobby hobby null,
  name varchar not null
);
```

You'd need to define the type in Haskell first:

```
module Example.Types
  ( Hobby (..),
  )
where

import GHC.Read (Read)
import GHC.Show (Show)
import Hasql.Generator.Types (HasqlEnum)

data Hobby
  = Reading
  | Writing
  | PlayingSports
  deriving stock (Show, Read)

instance HasqlEnum Hobby
```

Note that the type must be made an instance of `HasqlEnum`. You can rely on the
default class definitions to serialize/deserialize values, or you can supply
your own.

Then it's just a matter of creating a `Map Text EnumConfig`, where the key is
the name of the enum in PostgreSQL, the `moduleName` is the module where the
type is defined, and `haskellType` is the name of the type:

```hs
import Data.List.NonEmpty (fromList)
import Data.Map.Strict (empty)
import Hasql.Generator (generate)
import Hasql.Generator.Types (QueryConfig (..))

generateCodecs :: IO (Either (NonEmpty (Text, QueryConfig)) ())
generateCodecs = do
  let schemaFile = "path/to/schema.sql"
      queryConfigs = fromList
        [ QueryConfig
          { inputFile = "path/to/query_to_get_user_info.sql"
          , outputLocation = "path/to/QueryToGetUserInfo.hs"
          , moduleName = "QueryToGetUserInfo"
          , functionName = "query"
          }
        ]
      enumConfigs = Map.fromList
        [ ( "hobby"
          , EnumConfig
            { moduleName = "Example.Types"
            , haskellType = "Hobby"
            }
          )

  generate migrationFiles queryConfigs enumConfigs
```

## Known Issues

There is currently no support for subqueries nor CTEs. Support for them may
be added at a later time.

## Development

### Building

```
make build
```

### Testing

```
make test
```
