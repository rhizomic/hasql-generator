module Hasql.Generator.Internal.Database.Sql.Parser
  ( parseAsExpression,
    parseJoins,
    parseLimit,
    parseParameters,
  )
where

import Control.Lens (preview, toListOf, traverse, view)
import Control.Monad ((=<<))
import Data.Bool (Bool (False, True))
import Data.Either (Either (Left, Right))
import Data.Eq ((==))
import Data.Foldable (concatMap, foldMap)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (null, (++))
import Data.List.NonEmpty (head, nonEmpty)
import Data.List.NonEmpty.Extra (NonEmpty)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe, mapMaybe, maybe)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhileEnd, intercalate, strip)
import GHC.Real (fromIntegral)
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( ColumnReference (ColumnReference, columnName, tableName),
    JoinInformation (JoinInformation, joinType, tableAndAlias),
    Parameter (Parameter, parameterNumber, parameterReference),
    PostgresqlExpression (ColumnExpression, ConstantExpression),
    PostgresqlJoinType (CrossJoin, FullJoin, InnerJoin, LeftJoin, RightJoin),
    TableAndAlias (TableAndAlias, alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import PgQuery
  ( A_Expr,
    ColumnRef,
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
    ResTarget,
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
    lexpr,
    limitCount,
    maybe'ival,
    maybe'node,
    maybe'val,
    name,
    number,
    quals,
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
import PostgresqlSyntax.Ast
  ( AExpr (CExprAExpr, TypecastAExpr),
    AexprConst (BoolAexprConst, FAexprConst, IAexprConst, SAexprConst),
    AliasClause (AliasClause),
    CExpr (AexprConstCExpr, ColumnrefCExpr),
    Columnref (Columnref),
    FromClause,
    Ident (QuotedIdent, UnquotedIdent),
    IndirectionEl (AttrNameIndirectionEl),
    JoinMeth (CrossJoinMeth, NaturalJoinMeth, QualJoinMeth),
    JoinType (FullJoinType, InnerJoinType, LeftJoinType, RightJoinType),
    JoinedTable (InParensJoinedTable, MethJoinedTable),
    PreparableStmt (CallPreparableStmt, DeletePreparableStmt, InsertPreparableStmt, SelectPreparableStmt, UpdatePreparableStmt),
    QualifiedName (IndirectedQualifiedName, SimpleQualifiedName),
    RelationExpr (OnlyRelationExpr, SimpleRelationExpr),
    SelectNoParens (SelectNoParens),
    SelectWithParens (NoParensSelectWithParens, WithParensSelectWithParens),
    SimpleSelect (BinSimpleSelect, NormalSimpleSelect, TableSimpleSelect, ValuesSimpleSelect),
    TableRef (FuncTableRef, JoinTableRef, RelationExprTableRef, SelectTableRef),
  )
import PostgresqlSyntax.Parsing (aExpr, preparableStmt, run)

parseAsExpression :: Text -> Maybe PostgresqlExpression
parseAsExpression text =
  case run aExpr text of
    Left _msg -> Nothing
    Right result -> parseAExpr result
  where
    parseAExpr :: AExpr -> Maybe PostgresqlExpression
    parseAExpr expr = case expr of
      CExprAExpr cexpr -> parseCExpr cexpr
      TypecastAExpr texpr _name -> parseAExpr texpr
      _ -> Nothing

    parseCExpr :: CExpr -> Maybe PostgresqlExpression
    parseCExpr expr = case expr of
      AexprConstCExpr cexpr -> case cexpr of
        IAexprConst _val ->
          -- e.g., `3`
          Just ConstantExpression
        FAexprConst _val ->
          -- e.g., `3.12`
          Just ConstantExpression
        SAexprConst _val ->
          -- e.g., `'foo'`
          Just ConstantExpression
        BoolAexprConst _val ->
          -- e.g., `true`
          Just ConstantExpression
        _ -> Nothing
      ColumnrefCExpr (Columnref columnId mIndirection) -> case mIndirection of
        Nothing ->
          Just . ColumnExpression $
            ColumnReference
              { tableName = Nothing
              , columnName = unwrapIdent columnId
              }
        Just indirectionEl ->
          case head indirectionEl of
            AttrNameIndirectionEl columnName ->
              Just . ColumnExpression $
                ColumnReference
                  { tableName = Just $ unwrapIdent columnId
                  , columnName = unwrapIdent columnName
                  }
            _ ->
              -- We deliberately don't want to support other attributes
              -- (e.g., `*`), as end users should be forced to be explicit
              -- about the columns they're requesting.
              Nothing
      _ ->
        Nothing

    unwrapIdent :: Ident -> Text
    unwrapIdent (QuotedIdent ident) = ident
    unwrapIdent (UnquotedIdent ident) = ident

parseJoins :: Text -> Maybe (NonEmpty TableRelation)
parseJoins text =
  case run preparableStmt query of
    Left _err -> Nothing
    Right result -> case result of
      InsertPreparableStmt _insertStatement -> Nothing
      UpdatePreparableStmt _updateStatement -> Nothing
      DeletePreparableStmt _deleteStatement -> Nothing
      CallPreparableStmt _callStatement -> Nothing
      SelectPreparableStmt selectStatement -> parseSelectStatement selectStatement
  where
    query :: Text
    query = dropWhileEnd (== ';') $ strip text

    parseSelectStatement ::
      Either SelectNoParens SelectWithParens ->
      Maybe (NonEmpty TableRelation)
    parseSelectStatement = \case
      Right selectWithParens -> parseSelectWithParens selectWithParens
      Left selectNoParens -> parseSelectNoParens selectNoParens

    parseSelectWithParens ::
      SelectWithParens ->
      Maybe (NonEmpty TableRelation)
    parseSelectWithParens = \case
      WithParensSelectWithParens withParens -> parseSelectWithParens withParens
      NoParensSelectWithParens noParens -> parseSelectNoParens noParens

    parseSelectNoParens ::
      SelectNoParens ->
      Maybe (NonEmpty TableRelation)
    parseSelectNoParens (SelectNoParens _mWithClause selectClause _mSortClause _mSelectLimit _mForLockingClause) =
      parseSelectClause selectClause

    parseSelectClause ::
      Either SimpleSelect SelectWithParens ->
      Maybe (NonEmpty TableRelation)
    parseSelectClause = \case
      Right withParens -> parseSelectWithParens withParens
      Left simpleSelect -> parseSimpleSelect simpleSelect

    parseSimpleSelect ::
      SimpleSelect ->
      Maybe (NonEmpty TableRelation)
    parseSimpleSelect = \case
      ValuesSimpleSelect _valuesClause -> Nothing
      TableSimpleSelect _relationExpr -> Nothing
      BinSimpleSelect _selectBinOp _selectClause1 _mBool _selectClause2 -> Nothing
      NormalSimpleSelect _mTargeting _mIntoClause mFromClause _mWhereClause _mGroupClause _mHavingClause _mWindowClause ->
        parseFromClause =<< mFromClause

    parseFromClause ::
      FromClause ->
      Maybe (NonEmpty TableRelation)
    parseFromClause = foldMap parseTableRefAsTableRelations

    parseTableRefAsTableRelations ::
      TableRef ->
      Maybe (NonEmpty TableRelation)
    parseTableRefAsTableRelations = \case
      RelationExprTableRef _relationExpr _mAliasClause _mTablesampleClause -> Nothing
      FuncTableRef _mBool _funcTable _mFuncAliasClause -> Nothing
      SelectTableRef _mBool _selectWithParens _mAliasClause -> Nothing
      JoinTableRef joinedTable _mAliasClause -> nonEmpty $ parseJoinedTable joinedTable

    parseJoinedTable ::
      JoinedTable ->
      [TableRelation]
    parseJoinedTable = \case
      InParensJoinedTable joinedTable -> parseJoinedTable joinedTable
      MethJoinedTable joinMeth tableRef1 tableRef2 ->
        let joinType = parseJoinMeth joinMeth
         in parseJoinTypeAndTableRefAsTableRelations Nothing tableRef1
              ++ parseJoinTypeAndTableRefAsTableRelations (Just joinType) tableRef2

    parseJoinTypeAndTableRefAsTableRelations ::
      Maybe PostgresqlJoinType ->
      TableRef ->
      [TableRelation]
    parseJoinTypeAndTableRefAsTableRelations mJoinType = \case
      FuncTableRef _mBool _funcTable _mFuncAliasClause -> []
      SelectTableRef _mBool _selectWithParens _mAliasClause -> []
      JoinTableRef joinedTable _mAliasClause -> parseJoinedTable joinedTable
      RelationExprTableRef relationExpr mAliasClause _mTablesampleClause ->
        let table = parseRelationExpr relationExpr
            -- If there's no alias, just re-use the table name.
            alias = maybe table parseAliasClause mAliasClause

            tableAndAlias =
              TableAndAlias
                { table
                , alias
                }
         in [toTableRelation tableAndAlias]
      where
        toTableRelation ::
          TableAndAlias ->
          TableRelation
        toTableRelation tableAndAlias =
          case mJoinType of
            Nothing ->
              BaseTable tableAndAlias
            Just joinType ->
              let joinInformation =
                    JoinInformation
                      { tableAndAlias
                      , joinType
                      }
               in JoinTable joinInformation

        parseRelationExpr ::
          RelationExpr ->
          Text
        parseRelationExpr = \case
          SimpleRelationExpr qualifiedName _asteriskPresent -> parseQualifiedName qualifiedName
          OnlyRelationExpr qualifiedName _parenthesesPresent -> parseQualifiedName qualifiedName

        parseQualifiedName ::
          QualifiedName ->
          Text
        parseQualifiedName = \case
          SimpleQualifiedName ident -> parseIdent ident
          IndirectedQualifiedName ident _indirection -> parseIdent ident

        parseIdent ::
          Ident ->
          Text
        parseIdent = \case
          QuotedIdent ident -> ident
          UnquotedIdent ident -> ident

        parseAliasClause ::
          AliasClause ->
          Text
        parseAliasClause (AliasClause _asPresent colId _mNameList) =
          parseIdent colId

    parseJoinMeth ::
      JoinMeth ->
      PostgresqlJoinType
    parseJoinMeth = \case
      CrossJoinMeth -> CrossJoin
      QualJoinMeth mJoinType _joinQual -> parseJoinType mJoinType
      NaturalJoinMeth mJoinType -> parseJoinType mJoinType

    parseJoinType ::
      Maybe JoinType ->
      PostgresqlJoinType
    parseJoinType = \case
      Nothing -> InnerJoin
      Just joinType -> case joinType of
        FullJoinType _outer -> FullJoin
        LeftJoinType _outer -> LeftJoin
        RightJoinType _outer -> RightJoin
        InnerJoinType -> InnerJoin

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

          parameters = concatMap nodeToParameters (targetLists ++ joinClauses ++ whereClauses)

          selectCtes = view (traverse . withClause . ctes) selectStatements
          updateCtes = view (traverse . withClause . ctes) updateStatements
          deleteCtes = view (traverse . withClause . ctes) deleteStatements
          insertCtes = view (traverse . withClause . ctes) insertStatements
          cteNodes = concatMap nodeToCommonTableExpressionNodes (selectCtes ++ updateCtes ++ deleteCtes ++ insertCtes)

          cteParameters = nodesToParameters cteNodes
       in parameters ++ cteParameters
      where
        nodeToCommonTableExpressionNodes :: Node -> [Node]
        nodeToCommonTableExpressionNodes subNode =
          case view maybe'node subNode of
            Just (Node'CommonTableExpr cte) -> [view ctequery cte]
            _ -> []

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
