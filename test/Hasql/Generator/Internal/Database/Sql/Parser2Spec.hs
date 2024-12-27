{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Hasql.Generator.Internal.Database.Sql.Parser2Spec (spec) where

import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sortBy)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (compare)
import Data.Text (unpack)
import Hasql.Generator.Internal.Database.Sql.Parser2 (parseLimit, parseParameters, parseResults, parseTableRelations)
import Hasql.Generator.Internal.Database.Sql.Parser2.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    Parameter (Parameter, parameterNumber, parameterReference),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin),
    Result (Result),
    TableAndAlias (TableAndAlias, alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import PgQuery (parseSql)
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty (shouldBe)
import TestImport.Assertions (assertRight)

spec :: Spec
spec = do
  describe "parseTableRelations" $ do
    describe "When given a select statement" $ do
      it "returns the correct results for a query that has no joins" $ do
        let query = "select * from users"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has an implicit inner join" $ do
        let query = "select email, line_1 from users join addresses on users.id = addresses.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = InnerJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has an explicit inner join" $ do
        let query = "select email, line_1 from users inner join addresses on users.id = addresses.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = InnerJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has an explicit left join" $ do
        let query = "select email, line_1 from users left join addresses on users.id = addresses.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has multiple types of joins" $ do
        let query =
              "select email, line_1, phone \
              \from users \
              \join addresses on users.id = addresses.user_id \
              \left join phone_numbers on users.id = phone_numbers.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "phone_numbers"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has multiple left joins" $ do
        let query =
              "select email, line_1, phone, use_2fa \
              \from users \
              \left join addresses on users.id = addresses.user_id \
              \left join phone_numbers on users.id = phone_numbers.user_id \
              \left join mfa_settings on phone_numbers.id = mfa_settings.phone_number_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "phone_numbers"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "mfa_settings"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has multiple joins and multiple aliases" $ do
        let query =
              "select email, line_1, phone, use_2fa \
              \from users u \
              \join addresses a on u.id = a.user_id \
              \left join phone_numbers p on u.id = p.user_id \
              \left join mfa_settings on p.id = mfa_settings.phone_number_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Just "u"
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Just "a"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "phone_numbers"
                          , alias = Just "p"
                          }
                    , joinType = LeftJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "mfa_settings"
                          , alias = Nothing
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has a full join" $ do
        let query =
              "select * \
              \from users \
              \full join addresses on addresses.user_id = users.id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = FullJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that has a cross join" $ do
        let query =
              "select * \
              \from users \
              \cross join addresses"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Nothing
                          }
                    , joinType = InnerJoin
                    }
              ]

        actual `shouldBe` expected

    describe "When given a delete statement" $ do
      it "returns the correct results for a query that has no joins" $ do
        let query = "delete from users"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via USING" $ do
        let query =
              "delete from users u \
              \using nicknames n, \
              \addresses a \
              \where u.name = n.full_name \
              \and a.user_id = u.id \
              \and a.postal_code = $1"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Just "u"
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "nicknames"
                          , alias = Just "n"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Just "a"
                          }
                    , joinType = InnerJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via USING and regular JOINs" $ do
        let query =
              "delete from users \
              \using users u \
              \join addresses a on a.user_id = u.id \
              \left outer join nicknames n on u.name = n.full_name \
              \where a.postal_code = $1 \
              \and n.last_name = $2"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "users"
                          , alias = Just "u"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Just "a"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "nicknames"
                          , alias = Just "n"
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

    describe "When given an update statement" $ do
      it "returns the correct results for a query that has no joins" $ do
        let query = "update users set name = $1"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via FROM" $ do
        let query =
              "update users u \
              \set name = n.full_name \
              \from nicknames n \
              \where n.user_id = u.id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Just "u"
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "nicknames"
                          , alias = Just "n"
                          }
                    , joinType = InnerJoin
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via FROM and regular JOINs" $ do
        let query =
              "update users u \
              \set name = n.full_name \
              \from nicknames n \
              \left join addresses a on n.user_id = a.user_id \
              \where n.user_id = u.id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Just "u"
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "nicknames"
                          , alias = Just "n"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "addresses"
                          , alias = Just "a"
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

    describe "When given an insert statement" $ do
      it "returns the correct results for a query that has no joins" $ do
        let query = "insert into users (email, name) values ($1, $2) returning id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that uses joins" $ do
        let query =
              "insert into users (email, name) \
              \select u.email, n.full_name \
              \from users u left join nicknames n \
              \on u.id = n.user_id and n.id is not null"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseTableRelations result
            expected =
              [ BaseTable $
                  TableAndAlias
                    { table = "users"
                    , alias = Nothing
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "users"
                          , alias = Just "u"
                          }
                    , joinType = InnerJoin
                    }
              , JoinTable $
                  JoinInformation
                    { tableAndAlias =
                        TableAndAlias
                          { table = "nicknames"
                          , alias = Just "n"
                          }
                    , joinType = LeftJoin
                    }
              ]

        actual `shouldBe` expected

  describe "parseLimit" do
    it "returns the correct results for a query that has no specified limit" $ do
      let query = "select * from users"
      result <- assertRight <$> parseSql (unpack query)

      let actual = parseLimit result
          expected = Nothing

      actual `shouldBe` expected

    it "returns the correct results for a query that has a specified limit" $ do
      let query = "select * from users join addresses a on users.id = addresses.user_id limit 5"
      result <- assertRight <$> parseSql (unpack query)

      let actual = parseLimit result
          expected = Just 5

      actual `shouldBe` expected

    it "returns the correct results for a query that has a specified limit of 1" $ do
      let query = "select * from users limit 1"
      result <- assertRight <$> parseSql (unpack query)

      let actual = parseLimit result
          expected = Just 1

      actual `shouldBe` expected

  describe "parseParameters" do
    it "Parses no parameters from a query that lacks parameters" $ do
      let query = "select u.name from users"
      result <- assertRight <$> parseSql (unpack query)

      let expected = []
          actual = sortParameters $ parseParameters result

      actual `shouldBe` expected

    describe "When given a select statement" $ do
      it "Parses the parameters from a where clause containing basic expressions" $ do
        let query = "select name from users where id = $1 or created_at > $2"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "id"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "created_at"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

      it "Parses the parameters from a where clause containing an 'in'" $ do
        let query = "select u.name from users u where u.id in ($1, $2)"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "u.id"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "u.id"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

      it "Parses the parameters from join clauses and where clauses" $ do
        let query = "select u.name, a.line_1 from users u left join addresses a on u.id = a.user_id and a.city = $1 where u.id = $2 or a.line_2 = $3"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "a.city"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "u.id"
                  }
              , Parameter
                  { parameterNumber = 3
                  , parameterReference = "a.line_2"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

    describe "When given an update statement" $ do
      it "Parses the parameters from a where clause containing basic expressions" $ do
        let query = "update users set name = $1 where id = $2"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "name"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "id"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

      it "Parses the parameters from join clauses and where clauses" $ do
        let query =
              " update users u \
              \ set u.name = $1 \
              \ from addresses a \
              \   join nicknames n on n.user_id = u.id \
              \     and n.short_version = $2 \
              \     or n.long_version in ($3, $4) \
              \ where \
              \   a.user_id = u.id \
              \   and a.city = $5;"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "u.name"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "n.short_version"
                  }
              , Parameter
                  { parameterNumber = 3
                  , parameterReference = "n.long_version"
                  }
              , Parameter
                  { parameterNumber = 4
                  , parameterReference = "n.long_version"
                  }
              , Parameter
                  { parameterNumber = 5
                  , parameterReference = "a.city"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

    describe "When given a delete statement" $ do
      it "Parses the parameters from join clauses and where clauses" $ do
        let query =
              " delete from users \
              \ using users as u \
              \ left outer join nicknames n on \
              \   n.user_id = u.id \
              \   and n.long_version = $1 \
              \ where \
              \   users.id = u.id  \
              \   and n.short_version != $2;"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "n.long_version"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "n.short_version"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

    describe "When given an insert statement" $ do
      it "Parses the parameters from join clauses and where clauses" $ do
        let query =
              " insert into users (name) \
              \ select long_version from nicknames n \
              \   join preferences p on p.casual_name = n.short_version \
              \ where n.short_version = $1 \
              \   and p.use_dark_mode = $2;"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "n.short_version"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "p.use_dark_mode"
                  }
              ]
            actual = sortParameters $ parseParameters result

        actual `shouldBe` expected

  describe "parseResults" $ do
    describe "When given a select statement" $ do
      it "returns the correct results for a query that makes no use of aliases or joins" $ do
        let query = "select id, name from users"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              , Result "name"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that makes use of aliases" $ do
        let query = "select u.id, u.name from users u"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "u.id"
              , Result "u.name"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that uses joins" $ do
        let query =
              "select id, full_name \
              \from users \
              \join nicknames on users.id = nicknames.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              , Result "full_name"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that uses aliases and joins" $ do
        let query =
              "select u.id, n.full_name \
              \from users u \
              \join nicknames n on u.id = n.user_id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "u.id"
              , Result "n.full_name"
              ]

        actual `shouldBe` expected

    describe "When given a delete statement" $ do
      it "returns the correct results for a query that doesn't return anything" $ do
        let query = "delete from users"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected = []

        actual `shouldBe` expected

      it "returns the correct results for a query that makes no use of aliases or joins" $ do
        let query = "delete from users returning id, email"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              , Result "email"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via USING and regular JOINs" $ do
        let query =
              "delete from users \
              \using users u \
              \join addresses a on a.user_id = u.id \
              \left outer join nicknames n on u.name = n.full_name \
              \where a.postal_code = $1 \
              \and n.last_name = $2 \
              \returning u.id, a.postal_code, n.first_name"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "u.id"
              , Result "a.postal_code"
              , Result "n.first_name"
              ]

        actual `shouldBe` expected

    describe "When given an update statement" $ do
      it "returns the correct results for a query that doesn't return anything" $ do
        let query = "update users set name = $1"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected = []

        actual `shouldBe` expected

      it "returns the correct results for a query that makes no use of aliases or joins" $ do
        let query = "update users set name = $1 returning id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via FROM" $ do
        let query =
              "update users u \
              \set name = n.full_name \
              \from nicknames n \
              \where n.user_id = u.id \
              \returning u.id, u.name"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "u.id"
              , Result "u.name"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that joins via FROM and regular JOINs" $ do
        let query =
              "update users u \
              \set name = n.full_name \
              \from nicknames n \
              \left join addresses a on n.user_id = a.user_id \
              \where n.user_id = u.id \
              \returning u.id, u.name, u.created_at"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "u.id"
              , Result "u.name"
              , Result "u.created_at"
              ]

        actual `shouldBe` expected

    describe "When given an insert statement" $ do
      it "returns the correct results for a query that has no joins" $ do
        let query = "insert into users (email, name) values ($1, $2) returning id"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              ]

        actual `shouldBe` expected

      it "returns the correct results for a query that uses joins" $ do
        let query =
              "insert into users (email, name) \
              \select u.email, n.full_name \
              \from users u left join nicknames n \
              \on u.id = n.user_id and n.id is not null \
              \returning id, email, name"
        result <- assertRight <$> parseSql (unpack query)

        let actual = parseResults result
            expected =
              [ Result "id"
              , Result "email"
              , Result "name"
              ]
        actual `shouldBe` expected

sortParameters :: [Parameter] -> [Parameter]
sortParameters = sortBy (\x y -> compare x.parameterNumber y.parameterNumber)
