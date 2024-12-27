module Hasql.Generator.Internal.Database.Sql.Parser2
  ( parseLimit,
    parseParameters,
    parseResults,
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
import Data.List (null, (++))
import Data.List.NonEmpty (head, nonEmpty)
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
import Hasql.Generator.Internal.Database.Sql.Parser2.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    Parameter (Parameter, parameterNumber, parameterReference),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin, RightJoin),
    Result (Result),
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
        Node'ResTarget,
        Node'SubLink
      ),
    ParamRef,
    ParseResult,
    RangeVar,
    ResTarget,
    SelectStmt,
    UpdateStmt,
    aliasname,
    args,
    deleteStmt,
    fields,
    fromClause,
    indirection,
    insertStmt,
    items,
    ival,
    joinExpr,
    jointype,
    larg,
    limitCount,
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
    subselect,
    sval,
    targetList,
    updateStmt,
    usingClause,
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

parseParameters ::
  ParseResult ->
  [Parameter]
parseParameters result =
  let statements = toListOf (stmts . traverse . stmt) result
   in nodesToParameters statements
  where
    nodesToParameters :: [Node] -> [Parameter]
    nodesToParameters [] = []
    nodesToParameters statements =
      let selectStatements = fmap (view selectStmt) statements
          selectFromClauses = view (traverse . fromClause) selectStatements
          selectWhereClauses = fmap (view whereClause) selectStatements

          updateStatements = fmap (view updateStmt) statements
          updateTargetList = view (traverse . targetList) updateStatements
          updateFromClauses = view (traverse . fromClause) updateStatements
          updateWhereClauses = fmap (view whereClause) updateStatements

          deleteStatements = fmap (view deleteStmt) statements
          deleteUsingClauses = view (traverse . usingClause) deleteStatements
          deleteWhereClauses = fmap (view whereClause) deleteStatements

          insertStatements = fmap (view insertStmt) statements
          insertSelectStatements = fmap (view (selectStmt . selectStmt)) insertStatements
          insertSelectFromClauses = view (traverse . fromClause) insertSelectStatements
          insertSelectWhereClauses = fmap (view whereClause) insertSelectStatements

          joinClauses =
            fmap
              (view (joinExpr . quals))
              (selectFromClauses ++ updateFromClauses ++ deleteUsingClauses ++ insertSelectFromClauses)
          whereClauses = selectWhereClauses ++ updateWhereClauses ++ deleteWhereClauses ++ insertSelectWhereClauses
       in concatMap nodeToParameters (updateTargetList ++ joinClauses ++ whereClauses)

    nodeToParameters :: Node -> [Parameter]
    nodeToParameters subNode =
      case view maybe'node subNode of
        Just (Node'BoolExpr expr) ->
          let boolArgs = view args expr
           in concatMap nodeToParameters boolArgs
        Just (Node'AExpr expr) ->
          aExprToParameters expr
        Just (Node'ResTarget target) ->
          resTargetToParameters target
        Just (Node'SubLink subLink) ->
          nodesToParameters $ toListOf subselect subLink
        _ -> []

    aExprToParameters :: A_Expr -> [Parameter]
    aExprToParameters aExpression =
      case (view maybe'lexpr aExpression, view maybe'rexpr aExpression) of
        (Just leftNode, Just rightNode) ->
          case (view maybe'node leftNode, view maybe'node rightNode) of
            (Just (Node'ColumnRef columnRef), Just (Node'ParamRef paramRef)) ->
              [toParameter columnRef paramRef]
            (Just (Node'ParamRef paramRef), Just (Node'ColumnRef columnRef)) ->
              [toParameter columnRef paramRef]
            (Just (Node'ColumnRef columnRef), Just (Node'List list)) ->
              fmap (toParameter columnRef) $ listToParamRefs list
            (Just (Node'List list), Just (Node'ColumnRef columnRef)) ->
              fmap (toParameter columnRef) $ listToParamRefs list
            _ ->
              []
        _ ->
          []
      where
        toParameter ::
          ColumnRef ->
          ParamRef ->
          Parameter
        toParameter columnRef paramRef =
          let parameterNumber = fromIntegral (view number paramRef)
              allFields = view fields columnRef
              parameterReference = intercalate "." (fmap nodeToText allFields)
           in Parameter
                { parameterNumber
                , parameterReference
                }

    listToParamRefs ::
      List ->
      [ParamRef]
    listToParamRefs list =
      mapMaybe (view maybe'paramRef) (view items list)

    resTargetToParameters :: ResTarget -> [Parameter]
    resTargetToParameters target =
      case view maybe'paramRef =<< view maybe'val target of
        Just paramRef ->
          let subNodes = view indirection target
              suffix = case null subNodes of
                True -> ""
                False -> "." <> intercalate "." (fmap nodeToText subNodes)
           in [ Parameter
                  { parameterNumber = fromIntegral (view number paramRef)
                  , parameterReference = view name target <> suffix
                  }
              ]
        Nothing ->
          []

parseResults ::
  ParseResult ->
  [Result]
parseResults result =
  let allStatements = toListOf (stmts . traverse . stmt) result
   in case nonEmpty allStatements of
        Just statements -> nodeToResults $ head statements
        Nothing -> []
  where
    nodeToResults :: Node -> [Result]
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
        getResultsFromSelect :: SelectStmt -> [Result]
        getResultsFromSelect selectStatement =
          let targetLists = view targetList selectStatement
              resTargets = fmap (view resTarget) targetLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromDelete :: DeleteStmt -> [Result]
        getResultsFromDelete deleteStatement =
          let returningLists = view returningList deleteStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromUpdate :: UpdateStmt -> [Result]
        getResultsFromUpdate updateStatement =
          let returningLists = view returningList updateStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        getResultsFromInsert :: InsertStmt -> [Result]
        getResultsFromInsert insertStatement =
          let returningLists = view returningList insertStatement
              resTargets = fmap (view resTarget) returningLists
           in mapMaybe resTargetToResult resTargets

        resTargetToResult :: ResTarget -> Maybe Result
        resTargetToResult target =
          case view maybe'columnRef =<< view maybe'val target of
            Nothing -> Nothing
            Just columnRef ->
              let allFields = view fields columnRef
                  columnName = intercalate "." (fmap nodeToText allFields)
               in Just $ Result columnName

parseTableRelations ::
  ParseResult ->
  [TableRelation]
parseTableRelations result =
  let allStatements = toListOf (stmts . traverse . stmt) result
   in case nonEmpty allStatements of
        Just statements -> nodeToTableRelations $ head statements
        Nothing -> []
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
