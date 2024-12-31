module Hasql.Generator.Internal.Database.Sql.Parser
  ( parseLimit,
    parseQueryParameters,
    parseQueryResults,
    parseTableRelations,
  )
where

import Control.Lens (toListOf, traverse, view)
import Control.Monad ((=<<))
import Data.Bool (Bool (False, True))
import Data.Foldable (concatMap)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (null, sort, zip, (++))
import Data.List.NonEmpty (NonEmpty, head, nonEmpty)
import Data.Maybe
  ( Maybe (Just, Nothing),
    catMaybes,
    listToMaybe,
    mapMaybe,
    maybe,
    maybeToList,
  )
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import GHC.Real (fromIntegral)
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin, RightJoin),
    QueryParameter (QueryParameter, parameterNumber, parameterReference),
    QueryResult (QueryResult),
    TableAndAlias (TableAndAlias, alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import PgQuery
  ( A_Expr,
    ColumnRef,
    DeleteStmt,
    InsertStmt,
    JoinExpr,
    JoinType
      ( JOIN_ANTI,
        JOIN_FULL,
        JOIN_INNER,
        JOIN_LEFT,
        JOIN_RIGHT,
        JOIN_RIGHT_ANTI,
        JOIN_SEMI,
        JOIN_TYPE_UNDEFINED,
        JOIN_UNIQUE_INNER,
        JOIN_UNIQUE_OUTER,
        JoinType'Unrecognized
      ),
    List,
    Node,
    Node'Node
      ( Node'AExpr,
        Node'BoolExpr,
        Node'ColumnRef,
        Node'List,
        Node'ParamRef,
        Node'ResTarget
      ),
    ParamRef,
    ParseResult,
    RangeVar,
    ResTarget,
    SelectStmt,
    UpdateStmt,
    aliasname,
    args,
    cols,
    fields,
    fromClause,
    indirection,
    items,
    ival,
    joinExpr,
    jointype,
    larg,
    limitCount,
    list,
    maybe'aConst,
    maybe'alias,
    maybe'columnRef,
    maybe'deleteStmt,
    maybe'insertStmt,
    maybe'ival,
    maybe'joinExpr,
    maybe'lexpr,
    maybe'node,
    maybe'paramRef,
    maybe'rexpr,
    maybe'selectStmt,
    maybe'string,
    maybe'updateStmt,
    maybe'val,
    name,
    number,
    quals,
    rangeVar,
    rarg,
    relation,
    relname,
    resTarget,
    returningList,
    selectStmt,
    stmt,
    stmts,
    sval,
    targetList,
    usingClause,
    valuesLists,
    whereClause,
  )

parseLimit ::
  ParseResult ->
  Maybe Int
parseLimit result =
  let mLimitNode = listToMaybe $ toListOf (stmts . traverse . stmt . selectStmt . limitCount) result
   in nodeToConstInteger =<< mLimitNode
  where
    nodeToConstInteger :: Node -> Maybe Int
    nodeToConstInteger subNode =
      fromIntegral . view ival
        <$> (view maybe'ival =<< view maybe'aConst subNode)

parseQueryParameters ::
  ParseResult ->
  Maybe (NonEmpty QueryParameter)
parseQueryParameters result =
  let allStatements = toListOf (stmts . traverse . stmt) result
      results = case nonEmpty allStatements of
        Just statements -> sort . nodesToParameters $ head statements
        Nothing -> []
   in nonEmpty results
  where
    nodesToParameters :: Node -> [QueryParameter]
    nodesToParameters statement =
      let mSelectStatement = view maybe'selectStmt statement
          selectParameters = maybe [] getParametersFromSelect mSelectStatement

          mDeleteStatement = view maybe'deleteStmt statement
          deleteParameters = maybe [] getParametersFromDelete mDeleteStatement

          mUpdateStatement = view maybe'updateStmt statement
          updateParameters = maybe [] getParametersFromUpdate mUpdateStatement

          mInsertStatement = view maybe'insertStmt statement
          insertParameters = maybe [] getParametersFromInsert mInsertStatement
       in selectParameters ++ deleteParameters ++ updateParameters ++ insertParameters
      where
        getParametersFromSelect :: SelectStmt -> [QueryParameter]
        getParametersFromSelect selectStatement =
          let selectWhereClause = view whereClause selectStatement

              selectFromClauses = view fromClause selectStatement
              joinClauses = fmap (view (joinExpr . quals)) selectFromClauses
           in concatMap nodeToParameters (selectWhereClause : joinClauses)

        getParametersFromDelete :: DeleteStmt -> [QueryParameter]
        getParametersFromDelete deleteStatement =
          let deleteWhereClause = view whereClause deleteStatement

              deleteUsingClauses = view usingClause deleteStatement
              joinClauses = fmap (view (joinExpr . quals)) deleteUsingClauses
           in concatMap nodeToParameters (deleteWhereClause : joinClauses)

        getParametersFromUpdate :: UpdateStmt -> [QueryParameter]
        getParametersFromUpdate updateStatement =
          let updateWhereClause = view whereClause updateStatement
              updateTargetList = view targetList updateStatement

              updateFromClauses = view fromClause updateStatement
              joinClauses = fmap (view (joinExpr . quals)) updateFromClauses
           in concatMap nodeToParameters (updateWhereClause : (updateTargetList ++ joinClauses))

        getParametersFromInsert :: InsertStmt -> [QueryParameter]
        getParametersFromInsert insertStatement =
          let insertSelectWhereClause :: Node = view whereClause insertSelectStatement

              insertSelectFromClauses :: [Node] = view fromClause insertSelectStatement
              joinClauses =
                fmap
                  (view (joinExpr . quals))
                  insertSelectFromClauses
           in queryParametersFromInsertValues
                ++ concatMap nodeToParameters (insertSelectWhereClause : joinClauses)
          where
            insertSelectStatement :: SelectStmt
            insertSelectStatement = view (selectStmt . selectStmt) insertStatement

            queryParametersFromInsertValues :: [QueryParameter]
            queryParametersFromInsertValues =
              let insertColumns = view cols insertStatement
                  insertColumnNames = fmap (view (resTarget . name)) insertColumns

                  insertValueLists :: [Node] = view valuesLists insertSelectStatement
                  insertValuesItems = case nonEmpty insertValueLists of
                    Nothing -> []
                    Just valueList -> view (list . items) $ head valueList

                  columnNamesAndValues = zip insertColumnNames insertValuesItems
               in mapMaybe toParameter columnNamesAndValues
              where
                toParameter :: (Text, Node) -> Maybe QueryParameter
                toParameter (parameterReference, node) =
                  case view maybe'paramRef node of
                    Nothing -> Nothing
                    Just paramRef ->
                      let parameterNumber = fromIntegral (view number paramRef)
                       in Just
                            QueryParameter
                              { parameterNumber
                              , parameterReference
                              }

    nodeToParameters :: Node -> [QueryParameter]
    nodeToParameters subNode =
      case view maybe'node subNode of
        Just (Node'BoolExpr expr) ->
          let boolArgs = view args expr
           in concatMap nodeToParameters boolArgs
        Just (Node'AExpr expr) ->
          aExprToParameters expr
        Just (Node'ResTarget target) ->
          resTargetToParameters target
        _ -> []

    aExprToParameters :: A_Expr -> [QueryParameter]
    aExprToParameters aExpression =
      case (view maybe'lexpr aExpression, view maybe'rexpr aExpression) of
        (Just leftNode, Just rightNode) ->
          case (view maybe'node leftNode, view maybe'node rightNode) of
            (Just (Node'ColumnRef columnRef), Just (Node'ParamRef paramRef)) ->
              [toParameter columnRef paramRef]
            (Just (Node'ParamRef paramRef), Just (Node'ColumnRef columnRef)) ->
              [toParameter columnRef paramRef]
            (Just (Node'ColumnRef columnRef), Just (Node'List nodeList)) ->
              fmap (toParameter columnRef) $ listToParamRefs nodeList
            (Just (Node'List nodeList), Just (Node'ColumnRef columnRef)) ->
              fmap (toParameter columnRef) $ listToParamRefs nodeList
            _ ->
              []
        _ ->
          []
      where
        toParameter ::
          ColumnRef ->
          ParamRef ->
          QueryParameter
        toParameter columnRef paramRef =
          let parameterNumber = fromIntegral (view number paramRef)
              allFields = view fields columnRef
              parameterReference = intercalate "." (fmap nodeToText allFields)
           in QueryParameter
                { parameterNumber
                , parameterReference
                }

    listToParamRefs ::
      List ->
      [ParamRef]
    listToParamRefs lst =
      mapMaybe (view maybe'paramRef) (view items lst)

    resTargetToParameters :: ResTarget -> [QueryParameter]
    resTargetToParameters target =
      case view maybe'paramRef =<< view maybe'val target of
        Just paramRef ->
          let subNodes = view indirection target
              suffix = case null subNodes of
                True -> ""
                False -> "." <> intercalate "." (fmap nodeToText subNodes)
           in [ QueryParameter
                  { parameterNumber = fromIntegral (view number paramRef)
                  , parameterReference = view name target <> suffix
                  }
              ]
        Nothing ->
          []

parseQueryResults ::
  ParseResult ->
  Maybe (NonEmpty QueryResult)
parseQueryResults result =
  let allStatements = toListOf (stmts . traverse . stmt) result
      results = case nonEmpty allStatements of
        Just statements -> nodeToResults $ head statements
        Nothing -> []
   in nonEmpty results
  where
    nodeToResults :: Node -> [QueryResult]
    nodeToResults statement =
      let mSelectStatement = view maybe'selectStmt statement
          selectResults = maybe [] getResultsFromSelect mSelectStatement

          mDeleteStatement = view maybe'deleteStmt statement
          deleteResults = maybe [] getResultsFromDelete mDeleteStatement

          mUpdateStatement = view maybe'updateStmt statement
          updateResults = maybe [] getResultsFromUpdate mUpdateStatement

          mInsertStatement = view maybe'insertStmt statement
          insertResults = maybe [] getResultsFromInsert mInsertStatement
       in selectResults ++ deleteResults ++ updateResults ++ insertResults
      where
        getResultsFromSelect :: SelectStmt -> [QueryResult]
        getResultsFromSelect selectStatement =
          let targetLists = view targetList selectStatement
              resTargets = fmap (view resTarget) targetLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromDelete :: DeleteStmt -> [QueryResult]
        getResultsFromDelete deleteStatement =
          let returningLists = view returningList deleteStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromUpdate :: UpdateStmt -> [QueryResult]
        getResultsFromUpdate updateStatement =
          let returningLists = view returningList updateStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromInsert :: InsertStmt -> [QueryResult]
        getResultsFromInsert insertStatement =
          let returningLists = view returningList insertStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        resTargetToResult :: ResTarget -> Maybe QueryResult
        resTargetToResult target =
          case view maybe'columnRef =<< view maybe'val target of
            Nothing -> Nothing
            Just columnRef ->
              let allFields = view fields columnRef
                  columnName = intercalate "." (fmap nodeToText allFields)
               in Just $ QueryResult columnName

parseTableRelations ::
  ParseResult ->
  Maybe (NonEmpty TableRelation)
parseTableRelations result =
  let allStatements = toListOf (stmts . traverse . stmt) result
      relations = case nonEmpty allStatements of
        Just statements -> nodeToTableRelations $ head statements
        Nothing -> []
   in nonEmpty relations
  where
    nodeToTableRelations :: Node -> [TableRelation]
    nodeToTableRelations statement =
      let mSelectStatement = view maybe'selectStmt statement
          selectRelations = maybe [] getRelationsFromSelect mSelectStatement

          mDeleteStatement = view maybe'deleteStmt statement
          deleteRelations = maybe [] getRelationsFromDelete mDeleteStatement

          mUpdateStatement = view maybe'updateStmt statement
          updateRelations = maybe [] getRelationsFromUpdate mUpdateStatement

          mInsertStatement = view maybe'insertStmt statement
          insertRelations = maybe [] getRelationsFromInsert mInsertStatement
       in selectRelations ++ deleteRelations ++ updateRelations ++ insertRelations
      where
        getRelationsFromSelect :: SelectStmt -> [TableRelation]
        getRelationsFromSelect selectStatement =
          -- We only care about the first `from` clause.
          let selectFromClause = head <$> nonEmpty (view fromClause selectStatement)
           in maybe [] selectTableRelations selectFromClause
          where
            selectTableRelations :: Node -> [TableRelation]
            selectTableRelations fromClauseNode =
              case view maybe'joinExpr fromClauseNode of
                Nothing ->
                  let tableAlias = rangeVarToTableAlias $ view rangeVar fromClauseNode
                   in [BaseTable tableAlias]
                Just joinExpression ->
                  joinExpressionToTableRelations joinExpression

            joinExpressionToTableRelations :: JoinExpr -> [TableRelation]
            joinExpressionToTableRelations joinExpression =
              let leftArg = view larg joinExpression
               in case view maybe'joinExpr leftArg of
                    Nothing ->
                      let leftRangeVar = view rangeVar leftArg
                          baseTableAlias = rangeVarToTableAlias leftRangeVar
                          joinTable = maybeToList rightArgToTableRelation
                       in BaseTable baseTableAlias : joinTable
                    Just join ->
                      joinExpressionToTableRelations join
                        ++ maybeToList rightArgToTableRelation
              where
                rightArgToTableRelation :: Maybe TableRelation
                rightArgToTableRelation =
                  let rightRangeVar = view (rarg . rangeVar) joinExpression
                      joinTableAlias = rangeVarToTableAlias rightRangeVar
                      mJoinInfo = toJoinInformation (view jointype joinExpression) joinTableAlias
                   in JoinTable <$> mJoinInfo

        getRelationsFromDelete :: DeleteStmt -> [TableRelation]
        getRelationsFromDelete deleteStatement =
          let relationRangeVar = view relation deleteStatement
              baseTable = BaseTable $ rangeVarToTableAlias relationRangeVar

              usingClauses = view usingClause deleteStatement
              joinTables = concatMap (catMaybes . clauseToTableRelations) usingClauses
           in baseTable : joinTables

        getRelationsFromUpdate :: UpdateStmt -> [TableRelation]
        getRelationsFromUpdate updateStatement =
          let relationRangeVar = view relation updateStatement
              baseTable = BaseTable $ rangeVarToTableAlias relationRangeVar

              fromClauses = view fromClause updateStatement
              joinTables = concatMap (catMaybes . clauseToTableRelations) fromClauses
           in baseTable : joinTables

        getRelationsFromInsert :: InsertStmt -> [TableRelation]
        getRelationsFromInsert insertStatement =
          let relationRangeVar = view relation insertStatement
              baseTable = BaseTable $ rangeVarToTableAlias relationRangeVar

              mSelectFromClauses =
                nonEmpty . view fromClause
                  =<< view maybe'selectStmt
                  =<< view maybe'selectStmt insertStatement
              -- We only care about the first `from` clause.
              mSelectFromClause = head <$> mSelectFromClauses

              joinTables = catMaybes $ maybe [] clauseToTableRelations mSelectFromClause
           in baseTable : joinTables

    clauseToTableRelations :: Node -> [Maybe TableRelation]
    clauseToTableRelations clause =
      case view maybe'joinExpr clause of
        Nothing ->
          [ JoinTable
              <$> toJoinInformation
                JOIN_INNER
                (rangeVarToTableAlias $ view rangeVar clause)
          ]
        Just joinExpression ->
          Just <$> joinExpressionToTableRelations joinExpression
      where
        joinExpressionToTableRelations :: JoinExpr -> [TableRelation]
        joinExpressionToTableRelations joinExpression =
          let leftArg = view larg joinExpression
           in case view maybe'joinExpr leftArg of
                Nothing ->
                  let leftTable =
                        JoinTable
                          <$> toJoinInformation
                            JOIN_INNER
                            (rangeVarToTableAlias $ view rangeVar leftArg)
                   in catMaybes [leftTable, rightArgToTableRelation]
                Just join ->
                  joinExpressionToTableRelations join
                    ++ maybeToList rightArgToTableRelation
          where
            rightArgToTableRelation :: Maybe TableRelation
            rightArgToTableRelation =
              let rightRangeVar = view (rarg . rangeVar) joinExpression
                  joinTableAlias = rangeVarToTableAlias rightRangeVar
                  mJoinInfo = toJoinInformation (view jointype joinExpression) joinTableAlias
               in JoinTable <$> mJoinInfo

    rangeVarToTableAlias :: RangeVar -> TableAndAlias
    rangeVarToTableAlias rVar =
      let table = view relname rVar
          alias = view aliasname <$> view maybe'alias rVar
       in TableAndAlias
            { table
            , alias
            }

    toJoinInformation :: JoinType -> TableAndAlias -> Maybe JoinInformation
    toJoinInformation jType tableAndAlias =
      case joinTypeToPostgresqlJoinType jType of
        Nothing ->
          Nothing
        Just joinType ->
          Just
            JoinInformation
              { tableAndAlias
              , joinType
              }

    joinTypeToPostgresqlJoinType :: JoinType -> Maybe PostgresqlJoinType
    joinTypeToPostgresqlJoinType = \case
      JOIN_INNER -> Just InnerJoin
      JOIN_LEFT -> Just LeftJoin
      JOIN_FULL -> Just FullJoin
      JOIN_RIGHT -> Just RightJoin
      JOIN_SEMI -> Nothing
      JOIN_ANTI -> Nothing
      JOIN_RIGHT_ANTI -> Nothing
      JOIN_UNIQUE_INNER -> Nothing
      JOIN_UNIQUE_OUTER -> Nothing
      JOIN_TYPE_UNDEFINED -> Nothing
      JoinType'Unrecognized _unrecognized -> Nothing

nodeToText :: Node -> Text
nodeToText subFieldNode =
  maybe "" (view sval) (view maybe'string subFieldNode)
