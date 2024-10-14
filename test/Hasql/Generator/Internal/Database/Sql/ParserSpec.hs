{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Hasql.Generator.Internal.Database.Sql.ParserSpec (spec) where

import Data.Function (($))
import Data.List.NonEmpty (fromList)
import Data.Maybe (Maybe (Just, Nothing))
import Hasql.Generator.Internal.Database.Sql.Parser (parseJoins, parseLimit)
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    PostgresqlJoinType (CrossJoin, FullJoin, InnerJoin, LeftJoin),
    TableAndAlias (TableAndAlias, alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
  describe "parseJoins" do
    it "returns the correct results for a query that has no joins" $ do
      let query = "select * from users"
          actual = parseJoins query

          expected = Nothing

      actual `shouldBe` expected

    it "returns the correct results for a query that has an implicit inner join" $ do
      let query = "select email, line_1 from users join addresses on users.id = addresses.user_id"
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
                            }
                      , joinType = InnerJoin
                      }
                ]

      actual `shouldBe` expected

    it "returns the correct results for a query that has an explicit inner join" $ do
      let query = "select email, line_1 from users inner join addresses on users.id = addresses.user_id"
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
                            }
                      , joinType = InnerJoin
                      }
                ]

      actual `shouldBe` expected

    it "returns the correct results for a query that has an explicit left join" $ do
      let query = "select email, line_1 from users left join addresses on users.id = addresses.user_id"
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
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
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
                            }
                      , joinType = InnerJoin
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "phone_numbers"
                            , alias = "phone_numbers"
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
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
                            }
                      , joinType = LeftJoin
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "phone_numbers"
                            , alias = "phone_numbers"
                            }
                      , joinType = LeftJoin
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "mfa_settings"
                            , alias = "mfa_settings"
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
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "u"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "a"
                            }
                      , joinType = InnerJoin
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "phone_numbers"
                            , alias = "p"
                            }
                      , joinType = LeftJoin
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "mfa_settings"
                            , alias = "mfa_settings"
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
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
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
          actual = parseJoins query

          expected =
            Just $
              fromList
                [ BaseTable $
                    TableAndAlias
                      { table = "users"
                      , alias = "users"
                      }
                , JoinTable $
                    JoinInformation
                      { tableAndAlias =
                          TableAndAlias
                            { table = "addresses"
                            , alias = "addresses"
                            }
                      , joinType = CrossJoin
                      }
                ]

      actual `shouldBe` expected

  describe "parseLimit" do
    it "returns the correct results for a query that has no specified limit" $ do
      let query = "select * from users"
          actual = parseLimit query
          expected = Nothing

      actual `shouldBe` expected

    it "returns the correct results for a query that has a specified limit" $ do
      let query = "select * from users join addresses a on users.id = addresses.user_id limit 5"
          actual = parseLimit query
          expected = Just 5

      actual `shouldBe` expected

    it "returns the correct results for a query that has a specified limit of 1" $ do
      let query = "select * from users limit 1"
          actual = parseLimit query
          expected = Just 1

      actual `shouldBe` expected
