module Hasql.Generator.Internal.RendererSpec (spec) where

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Map.Strict (empty)
import Data.String (String)
import Data.Text (Text, unpack)
import Hasql.Generator.Internal.Database.Sql.Analysis.Types
  ( ColumnMetadata (ColumnMetadata, columnNullConstraint, columnType),
    NullabilityConstraint (NotNull, Null),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        numberOfRowsReturned,
        parameterMetadata,
        resultMetadata
      ),
    PostgresqlType (PgText, PgUuid),
  )
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( NumberOfRowsReturned (AtMostMoreThanOne, AtMostOne, ExactlyOne, None, Unknown),
  )
import Hasql.Generator.Internal.Renderer (toHaskell)
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
              , numberOfRowsReturned = None
              }

          actual = toHaskell sql parameterAndResultMetadata "DeleteFromAddresses" "query" empty

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
              , numberOfRowsReturned = Unknown
              }

          actual = toHaskell sql parameterAndResultMetadata "SelectIdFromAddresses" "query" empty

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
              , numberOfRowsReturned = Unknown
              }

          actual = toHaskell sql parameterAndResultMetadata "SelectIdLine1AndLine2FromAddresses" "query" empty

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
              , numberOfRowsReturned = None
              }

          actual = toHaskell sql parameterAndResultMetadata "UpdateAddresses" "query" empty

      shouldBeGolden "one_param_no_results" actual

    it "renders the expected code when the metadata consists of one param and one result" $ do
      let sql :: ByteString = "select line_1 from addresses a where a.user_id = $1 limit 1;"
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
              , numberOfRowsReturned = AtMostOne
              }

          actual = toHaskell sql parameterAndResultMetadata "GetLine1Address" "query" empty

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
              , numberOfRowsReturned = Unknown
              }

          actual = toHaskell sql parameterAndResultMetadata "SelectIdPostalCodeAndCountryFromAddresses" "query" empty

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
              , numberOfRowsReturned = None
              }

          actual = toHaskell sql parameterAndResultMetadata "DeleteFromAddressesByPostalCodeAndCountry" "query" empty

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
              , numberOfRowsReturned = Unknown
              }

          actual = toHaskell sql parameterAndResultMetadata "SelectIdFromAddressesByUserIdAndCity" "query" empty

      shouldBeGolden "multiple_params_one_result" actual

    it "renders the expected code when the metadata consists of multiple params and multiple results" $ do
      let sql :: ByteString = "select id, line_1, line_2 from addresses where city = $1 or postal_code = $2 or country = $3 limit 5;"
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
              , numberOfRowsReturned = AtMostMoreThanOne
              }

          actual = toHaskell sql parameterAndResultMetadata "SelectIdLine1AndLine2FromAddressesByCityPostalCodeOrCountry" "query" empty

      shouldBeGolden "multiple_params_multiple_results" actual

    it "renders the expected code when the metadata consists of multiple params and one result and only one returned row is expected" $ do
      let sql :: ByteString = "insert into users (email, name) values ($1, $2) returning id"
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
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , numberOfRowsReturned = ExactlyOne
              }

          actual = toHaskell sql parameterAndResultMetadata "InsertUser" "query" empty

      shouldBeGolden "multiple_params_one_result_one_returned_row" actual

shouldBeGolden ::
  String ->
  Text ->
  Golden String
shouldBeGolden name contents =
  defaultGolden name (unpack contents)
