{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Hasql.Generator.Internal.Database.Sql.Parser2Spec (spec) where

import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sortBy)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (compare)
import Data.Text (unpack)
import Hasql.Generator.Internal.Database.Sql.Parser2 (parseLimit, parseParameters, parseTableRelations)
import Hasql.Generator.Internal.Database.Sql.Parser2.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    Parameter (Parameter, parameterNumber, parameterReference),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin),
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
  describe "parseTableRelations" do
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
        let query = "select u.name from users u where u.id = $1 or u.created_at > $2"
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "u.id"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "u.created_at"
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

      it "Parses the parameters from a CTE" $ do
        let query =
              "with regional_sales as ( \
              \    select region, sum(amount) as total_sales \
              \    from orders \
              \    where sale_metadata = $1 \
              \    group by region \
              \), top_regions AS ( \
              \    select region \
              \    from regional_sales \
              \    where total_sales > (select sum(total_sales) / 10 from regional_sales where region = $2) \
              \) \
              \select region, \
              \       product, \
              \       sum(quantity) as product_units, \
              \       sum(amount) as product_sales \
              \from orders \
              \where region in (select region from top_regions where postal_code = $3) \
              \group by region, product; "
        result <- assertRight <$> parseSql (unpack query)

        let expected =
              [ Parameter
                  { parameterNumber = 1
                  , parameterReference = "sale_metadata"
                  }
              , Parameter
                  { parameterNumber = 2
                  , parameterReference = "region"
                  }
              , Parameter
                  { parameterNumber = 3
                  , parameterReference = "postal_code"
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

sortParameters :: [Parameter] -> [Parameter]
sortParameters = sortBy (\x y -> compare x.parameterNumber y.parameterNumber)
