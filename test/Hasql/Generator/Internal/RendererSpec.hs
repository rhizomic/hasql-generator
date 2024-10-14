module Hasql.Generator.Internal.RendererSpec (spec) where

import Data.ByteString (ByteString)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Functor (fmap)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, intercalate, unpack)
import GHC.Base (error)
import Hasql.Generator.Internal.Database.Sql.Types
  ( ColumnMetadata
      ( ColumnMetadata,
        aliasName,
        columnName,
        columnNullConstraint,
        columnType,
        tableName
      ),
    FragmentMetadata
      ( ColumnFragment
      ),
    NullabilityConstraint (NotNull, Null),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        parameterMetadata,
        resultMetadata
      ),
    PostgresqlType
      ( PgText,
        PgUuid
      ),
  )
import Hasql.Generator.Internal.Renderer (renderingIssueToHuman, toHaskell)
import Hasql.Generator.Internal.Renderer.Types (RenderingIssue)
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "line_1"
                        , columnName = "id"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "line_2"
                        , columnName = "id"
                        , columnType = PgText
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "user_id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "postal_code"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "country"
                        , columnType = PgText
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "postal_code"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "country"
                        , columnType = PgText
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "user_id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "city"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
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
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "city"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "postal_code"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "country"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "addresses"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "line_1"
                        , columnName = "id"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "line_2"
                        , columnName = "id"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  ]
              }

          actual = toHaskell sql parameterAndResultMetadata Nothing "SelectIdLine1AndLine2FromAddressesByCityPostalCodeOrCountry" "query"

      shouldBeGolden "multiple_params_multiple_results" actual

shouldBeGolden ::
  String ->
  Either [RenderingIssue] Text ->
  Golden String
shouldBeGolden name = \case
  Left renderingIssues ->
    let issues = intercalate "\n" $ fmap renderingIssueToHuman renderingIssues
     in error ("Unexpected issue rendering " <> name <> ": " <> unpack issues)
  Right contents ->
    defaultGolden name (unpack contents)
