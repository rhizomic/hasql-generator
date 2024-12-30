module Hasql.Generator.Internal.Renderer2Spec (spec) where

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Maybe (Maybe (Just, Nothing))
import Data.String (String)
import Data.Text (Text, unpack)
import Hasql.Generator.Internal.Database.Sql.Analysis2.Types (ColumnMetadata (ColumnMetadata, columnNullConstraint, columnType), NullabilityConstraint (NotNull, Null), PostgresqlParameterAndResultMetadata (PostgresqlParameterAndResultMetadata, parameterMetadata, resultMetadata), PostgresqlType (PgText, PgUuid))
import Hasql.Generator.Internal.Renderer2 (toHaskell)
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Golden (Golden, defaultGolden)

spec :: Spec
spec = do
  describe "toHaskell" do
    it "renders the expected code when the metadata consists of no params and no results" $ do
      let sql :: ByteString = "delete from addresses;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata = []
              , resultMetadata = []
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "DeleteFromAddresses" "query"

      shouldBeGolden "no_params_no_results" actual

    it "renders the expected code when the metadata consists of no params and one result" $ do
      let sql :: ByteString = "select id from addresses;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata = []
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdFromAddresses" "query"

      shouldBeGolden "no_params_one_result" actual

    it "renders the expected code when the metadata consists of no params and multiple results" $ do
      let sql :: ByteString = "select id, line_1, line_2 from addresses;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata = []
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdLine1AndLine2FromAddresses" "query"

      shouldBeGolden "no_params_multiple_results" actual

    it "renders the expected code when the metadata consists of one param and no results" $ do
      let sql :: ByteString = "update addresses set line_2 = null where id = $1;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata = []
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "UpdateAddresses" "query"

      shouldBeGolden "one_param_no_results" actual

    it "renders the expected code when the metadata consists of one param and one result" $ do
      let sql :: ByteString = "select line_1 from addresses a where a.user_id = $1;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata (Just 1) "GetLine1Address" "query"

      shouldBeGolden "one_param_one_result" actual

    it "renders the expected code when the metadata consists of one param and multiple results" $ do
      let sql :: ByteString = "select id, postal_code, country from addresses where line_1 = $1;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdPostalCodeAndCountryFromAddresses" "query"

      shouldBeGolden "one_param_multiple_results" actual

    it "renders the expected code when the metadata consists of multiple params and no results" $ do
      let sql :: ByteString = "delete from addresses where postal_code = $1 and country = $2;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata = []
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "DeleteFromAddressesByPostalCodeAndCountry" "query"

      shouldBeGolden "multiple_params_no_results" actual

    it "renders the expected code when the metadata consists of multiple params and one result" $ do
      let sql :: ByteString = "select id from addresses where user_id = $1 and city = $2;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdFromAddressesByUserIdAndCity" "query"

      shouldBeGolden "multiple_params_one_result" actual

    it "renders the expected code when the metadata consists of multiple params and multiple results" $ do
      let sql :: ByteString = "select id, line_1, line_2 from addresses where city = $1 or postal_code = $2 or country = $3;"
          parameterAndResultMetadata =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdLine1AndLine2FromAddressesByCityPostalCodeOrCountry" "query"

      shouldBeGolden "multiple_params_multiple_results" actual

shouldBeGolden ::
  String ->
  Text ->
  Golden String
shouldBeGolden name contents =
  defaultGolden name (unpack contents)
