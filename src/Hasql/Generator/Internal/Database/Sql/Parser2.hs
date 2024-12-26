module Hasql.Generator.Internal.Database.Sql.Parser2
  ( parseLimit,
    parseParameters,
    parseTableRelations,
  )
where

import Control.Lens (preview, toListOf, traverse, view)
import Control.Monad ((=<<))
import Data.Bool (Bool (False, True))
import Data.Foldable (concatMap)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (null, (++))
import Data.List.NonEmpty (head, nonEmpty)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe, mapMaybe, maybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import GHC.Real (fromIntegral)
import Hasql.Generator.Internal.Database.Sql.Parser2.Types
  ( JoinInformation (JoinInformation, joinType, tableAndAlias),
    Parameter (Parameter, parameterNumber, parameterReference),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin, RightJoin),
    TableAndAlias (TableAndAlias, alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import PgQuery
  ( A_Expr,
    ColumnRef,
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
      ( Node'AConst,
        Node'AExpr,
        Node'BoolExpr,
        Node'ColumnRef,
        Node'CommonTableExpr,
        Node'List,
        Node'ParamRef,
        Node'ResTarget,
        Node'String,
        Node'SubLink
      ),
    ParamRef,
    ParseResult,
    RangeVar,
    ResTarget,
    aliasname,
    args,
    ctequery,
    ctes,
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
    lexpr,
    limitCount,
    maybe'alias,
    maybe'ival,
    maybe'joinExpr,
    maybe'node,
    maybe'selectStmt,
    maybe'val,
    name,
    number,
    quals,
    rangeVar,
    rarg,
    relname,
    rexpr,
    selectStmt,
    stmt,
    stmts,
    subselect,
    sval,
    targetList,
    updateStmt,
    usingClause,
    whereClause,
    withClause,
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
      case view maybe'node subNode of
        Just (Node'AConst aConst) -> fromIntegral . view ival <$> view maybe'ival aConst
        _ -> Nothing

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
      let selectStatements = toListOf (traverse . selectStmt) statements
          selectFromClauses = view (traverse . fromClause) selectStatements
          selectWhereClauses = toListOf (traverse . whereClause) selectStatements

          updateStatements = toListOf (traverse . updateStmt) statements
          updateTargetList = view (traverse . targetList) updateStatements
          updateFromClauses = view (traverse . fromClause) updateStatements
          updateWhereClauses = toListOf (traverse . whereClause) updateStatements

          deleteStatements = toListOf (traverse . deleteStmt) statements
          deleteUsingClauses = view (traverse . usingClause) deleteStatements
          deleteWhereClauses = toListOf (traverse . whereClause) deleteStatements

          insertStatements = toListOf (traverse . insertStmt) statements
          insertSelectStatements = toListOf (traverse . selectStmt . selectStmt) insertStatements
          insertSelectFromClauses = view (traverse . fromClause) insertSelectStatements
          insertSelectWhereClauses = toListOf (traverse . whereClause) insertSelectStatements

          -- TODO: Other target lists?
          targetLists = updateTargetList
          joinClauses =
            toListOf
              (traverse . joinExpr . quals)
              (selectFromClauses ++ updateFromClauses ++ deleteUsingClauses ++ insertSelectFromClauses)
          whereClauses = selectWhereClauses ++ updateWhereClauses ++ deleteWhereClauses ++ insertSelectWhereClauses
       in concatMap nodeToParameters (targetLists ++ joinClauses ++ whereClauses)

    nodeToParameters :: Node -> [Parameter]
    nodeToParameters subNode =
      case view maybe'node subNode of
        Just (Node'BoolExpr expr) ->
          let boolArgs = view args expr
           in concatMap nodeToParameters boolArgs
        Just (Node'AExpr expr) ->
          aExprToParameters expr
        Just (Node'ResTarget resTarget) ->
          resTargetToParameters resTarget
        Just (Node'SubLink subLink) ->
          nodesToParameters $ toListOf subselect subLink
        _ -> []

    aExprToParameters :: A_Expr -> [Parameter]
    aExprToParameters aExpression =
      case (preview lexpr aExpression, preview rexpr aExpression) of
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
            (_, Just (Node'SubLink subLink)) ->
              nodesToParameters $ toListOf subselect subLink
            (Just (Node'SubLink subLink), _) ->
              nodesToParameters $ toListOf subselect subLink
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
      mapMaybe nodeToParamRef (view items list)

    nodeToParamRef :: Node -> Maybe ParamRef
    nodeToParamRef node =
      case view maybe'node node of
        Just (Node'ParamRef paramRef) -> Just paramRef
        _ -> Nothing

    resTargetToParameters :: ResTarget -> [Parameter]
    resTargetToParameters resTarget =
      case view maybe'val resTarget of
        Nothing ->
          []
        Just node ->
          case view maybe'node node of
            Just (Node'ParamRef paramRef) ->
              let subNodes = view indirection resTarget
                  suffix = case null subNodes of
                    True -> ""
                    False -> "." <> intercalate "." (fmap nodeToText subNodes)
               in [ Parameter
                      { parameterNumber = fromIntegral (view number paramRef)
                      , parameterReference = view name resTarget <> suffix
                      }
                  ]
            _ ->
              []

    nodeToText :: Node -> Text
    nodeToText subFieldNode =
      case view maybe'node subFieldNode of
        Just (Node'String str) -> view sval str
        _ -> ""

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
          selectFromClause =
            case mSelectStatement of
              Nothing -> Nothing
              Just selectStatement -> head <$> nonEmpty (view fromClause selectStatement)
          -- TODO: Parse selectWhereClause

          selectRelations = maybe [] selectTableRelations selectFromClause
       in -- TODO: Other types of queries

          selectRelations

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
          let rightArg = view rarg joinExpression
              rightRangeVar = view rangeVar rightArg
              joinTableAlias = rangeVarToTableAlias rightRangeVar
              mJoinInfo = toJoinInformation joinTableAlias (view jointype joinExpression)
           in JoinTable <$> mJoinInfo

    rangeVarToTableAlias :: RangeVar -> TableAndAlias
    rangeVarToTableAlias rVar =
      let table = view relname rVar
          alias = view aliasname <$> view maybe'alias rVar
       in TableAndAlias
            { table
            , alias
            }

    toJoinInformation :: TableAndAlias -> JoinType -> Maybe JoinInformation
    toJoinInformation tableAndAlias jType =
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
